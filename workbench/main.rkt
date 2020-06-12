#lang racket/base
(require syntax/parse
         racket/string
         racket/match
         racket/list
         syntax/parse
         "../syntax.rkt"
         "../emit.rkt"
         (for-syntax racket/base
                     syntax/parse
                     "../syntax.rkt"))

(provide #%app)

(define-syntax-rule (go/string exprs ...)
  (go/emit (go/expand exprs ...)))

(define-syntax (go/write-file stx)
  (syntax-parse stx
    ((_ (~or* name:string name:id) exprs:expr ...)
     (syntax (with-output-to-file #:exists (quote replace)
               name
               (lambda () (display (go/string exprs ...))))))))


(go/write-file "main.go"
               (package main)
               (import fmt log strings
                       net sync time regexp
                       (cli "github/urfave/cli/v2")
                       "github.com/coreos/go-etcd/etcd"
                       "github.com/miekg/dns"
                       "github.com/corpix/revip")

               (var (DefaultEtcdHosts (slice string)
                      (create (slice string)
                              ("http://127.0.0.1:4001")))
                    (DefaultAddr string "0.0.0.0:5353")
                    (DefaultHealthQuery string "id.server.")

                    (Flags (slice cli.Flag)
                           (create (slice cli.Flag)
                                   (create (ref cli.StringFlag)
                                           ((Name "log-level")
                                            (Aliases (create (slice string) ("l")))
                                            (Usage "logging level (debug, info, error)")
                                            (Value "info"))))))

               (func (NewClient () ((client (ptr etcd.Client))))
                     (set client (etcd.NewClient DefaultEtcdHosts))
                     (client.SyncCluster)
                     (return client))

               ;;

               (type (Server (struct
                               (client       (ptr etcd.Client))
                               (addr          string)
                               (readTimeout   time.Duration)
                               (writeTimeout  time.Duration)
                               (group        (ptr sync.WaitGroup))
                               (router       (ptr Router))
                               (stop         (chan bool)))))

               (func (NewServer ((client (ptr etcd.Client))
                                 (addr    string))
                                ((server (ptr Server))))
                     (if (== addr "")
                         (set addr DefaultAddr))
                     (return (ref (create Server
                                          ((client  client)
                                           (addr    addr)
                                           (group  (new sync.WaitGroup))
                                           (router (NewRouter))
                                           (stop   (make (type (chan bool)))))))))

               (func ((Run (s (ptr Server))) () (error))
                     (def mux (dns.NewServeMux))
                     (mux.Handle "." s)
                     (s.group.Add 2)
                     (go (s.run mux "tcp"))
                     (go (s.run mux "udp"))

                     ;; Healthchecking.
                     (begin
                       (log.Printf "enabling health checking")
                       (go ((func () (for ()
                                       (time.Sleep (* 5 1e9))
                                       (s.HealthCheck))))))

                     ;; Set a Watch and check for changes.
                     (begin
                       (log.Printf "setting watch")
                       (def ch (make (type (chan (ptr etcd.Response)))))
                       (go ((func ()
                                  (go (s.client.Watch "/dnsrouter" 0 #t ch s.stop))
                                  (for () (select ((def n (receive ch))
                                                   (s.Update n))))))))
                     (begin
                       (log.Printf "getting initial list")
                       (def (n err) ((s.client.Get "/dnsrouter/" #f #t)))
                       (if (!= err nil) (s.Update n))
                       (log.Printf "ready for queries"))
                     (s.group.Wait)
                     (return nil))

               (func ((Stop (s (ptr Server))))
                     (send s.stop #t)
                     (s.group.Done)
                     (s.group.Done))

               (func ((Update (s (ptr Server))) ((e (ptr etcd.Response))))
                     (if (== nil e) (return))
                     ;; process the first and then loop over nodes
                     (def parts (strings.SplitN e.Node.Value "," 2))
                     (if (!= 2 (len parts))
                         (log.Printf "unable to parse node %s with value %s" e.Node.Key e.Node.Value)
                         (begin
                           (def err (s.router.Add (index parts 0) (index parts 1)))
                           (if (!= nil err) (log.Printf "unable to add %s" err))))
                     (for ((_ n) (range e.Node.Nodes))
                       (def parts (strings.SplitN n.Value "," 2))
                       (if (!= 2 (len parts))
                           (log.Printf "unable to parse node %s with value %s" n.Key n.Value)
                           (begin
                             (def err (s.router.Add (index parts 0) (index parts 1)))
                             (if (!= nil err) (log.Printf "unable to add %s" err))))))

               (func ((ServeDNS (s (ptr Server))) ((w dns.ResponseWriter) (req (ptr dns.Msg))))
                     (def q    (index (key req Question) 0))
                     (def name (strings.ToLower q.Name))
                     (if (or (== q.Qtype dns.TypeIXFR)
                             (== q.Qtype dns.TypeAXFR))
                         (begin (def m (new dns.Msg))
                                (m.SetRcode req dns.RcodeServerFailure)
                                (w.WriteMsg m)
                                (return)))

                     (def (servers err) ((s.router.Match name)))

                     (when (or (!= nil err)
                               (== 0 (len servers)))
                       (def m (new dns.Msg))
                       (m.SetRcode req dns.RcodeServerFailure)
                       (w.WriteMsg m)
                       (return))

                     (def serv (index servers
                                      (% (int (dns.Id))
                                         (len servers))))
                     (log.Printf "routing %s to %s" name serv)

                     (def c (new dns.Client))
                     (def (ret _ err) ((c.Exchange req serv)))
                     (when (!= nil err)
                       (def m (new dns.Msg))
                       (m.SetRcode req dns.RcodeServerFailure)
                       (w.WriteMsg m)
                       (return))

                     (w.WriteMsg ret))

               (func ((HealthCheck (s (ptr Server))))
                     (def (c m) ((new dns.Client)
                                 (new dns.Msg)))
                     (set (c.Net m.Question)
                          ("tcp" (make (type (slice dns.Question)) 1)))
                     (set (index m.Question 0)
                          (create dns.Question
                                  (DefaultHealthQuery dns.TypeTXT dns.ClassCHAOS)))
                     (for ((_ serv) (range (s.router.Servers)))
                       (when (or (not (check c m serv))
                                 (not (check c m serv)))
                         (log.Printf "healthcheck failed for %s" serv)
                         (s.router.RemoveServer serv))))

               (func ((run (s (ptr Server))) ((mux (ptr dns.ServeMux)) (net  string)))
                     (defer (s.group.Done))
                     (def server (ref (create dns.Server
                                              ((Addr s.addr)
                                               (Net net)
                                               (Handler mux)
                                               (ReadTimeout s.readTimeout)
                                               (WriteTimeout s.writeTimeout)))))
                     (def err (server.ListenAndServe))
                     (if (!= err nil) (log.Fatal err)))

               ;;

               (type (Router (struct
                               sync.RWMutex
                               (route (map string (slice string))))))

               (func (NewRouter () ((ptr Router)))
                     (def r (make (type (map string (slice string)))))
                     (return (ref (create Router ((route r))))))

               (func ((Add (r (ptr Router))) ((dest string) (re string))
                                             (error))
                     (r.Lock)
                     (defer (r.Unlock))
                     ;; For v6 this needs to be [ipv6]:port .
                     ;; Don't care about port here, just if the syntax is OK.)
                     (begin (def (ip _ err) ((net.SplitHostPort dest)))
                            (if (!= nil err) (return err))
                            (if (== nil (net.ParseIP ip))
                                (return (fmt.Errorf "not an IP address %s" dest))))
                     (begin (def (_ err) ((regexp.Compile re)))
                            (if (!= nil err) (return err))
                            (def (_ ok) ((index r.route re)))
                            (if (not ok)
                                (set (index r.route re)
                                     (make (type (slice string)) 0)))
                            (for ((_ d) (range (index r.route re)))
                              (if (== d dest)
                                  (begin (log.Printf "address %s already in list for %s" dest re)
                                         (return nil))))
                            (log.Printf "adding route %s for %s" re dest)
                            (set (index r.route re)
                                 (append (index r.route re) dest)))
                     (return nil))

               (func ((Remove (r (ptr Router)))
                      ((dest string) (re string)) (error))
                     (r.Lock)
                     (defer (r.Unlock))
                     (begin (def (_ err) ((regexp.Compile re)))
                            (if (!= nil err) (return err))
                            (def (_ ok) ((index r.route re)))
                            (if (not ok)
                                (return (fmt.Errorf "Regexp %s does not exist" re)))
                            (for ((i s) (range (index r.route re)))
                              (when (== s dest)
                                (log.Printf "removing %s" s)
                                (set (index r.route re)
                                     (append (slice (index r.route re) 0 i)
                                             (spread (slice (index r.route re) (+ 1 i)))))
                                (return nil))))
                     (return nil))

               (func ((RemoveServer (r (ptr Router))) ((serv string)))
                     (for ((rec servs) (range r.route))
                       (for ((_ serv1) (range servs))
                         (when (== serv1 serv)
                           (def err (r.Remove serv rec))
                           (if (!= nil err)
                               (log.Printf "%s" err))))))

               (func ((Match (r (ptr Router))) ((qname string)) ((slice string) error))
                     (r.RLock)
                     (defer (r.RUnlock))
                     (for ((re dest) (range r.route))
                       (def (ok _)
                         ((regexp.Match re (cast qname (slice byte)))))
                       (if ok (return dest nil)))
                     (return nil (fmt.Errorf "No match for %s" qname)))

               (func ((Servers (r (ptr Router))) () ((slice string)))
                     (r.RLock)
                     (defer (r.RUnlock))

                     (def s (make (type (slice string)) 0 5))
                     (for ((_ dest) (range r.route))
                       (set s (append s (spread dest))))
                     (return s))

               ;;

               (func (check ((c (ptr dns.Client))
                             (m (ptr dns.Msg))
                             (addr string))
                            (bool))
                     (set (key m Id) (dns.Id))
                     (def (in _ err) ((c.Exchange m addr)))
                     (if (!= nil err)                   (return false))
                     (if (!= in.Rcode dns.RcodeSuccess) (return false))
                     (return true))

               ;;

               (func (main)
                     (def s (NewServer (NewClient) DefaultAddr))
                     (def err (s.Run))
                     (if (!= err nil)
                         (log.Fatal err)))
               )
