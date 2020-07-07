#lang racket/base
(require syntax/parse
         racket/string
         racket/match
         racket/list
         syntax/parse
         "../main.rkt")

(go/write-file "main.go"
               (package main)
               (import os fmt log strings
                       net sync time regexp

                       (cli  github.com/urfave/cli/v2)
                       (yaml gopkg.in/yaml.v2)
                       github.com/coreos/go-etcd/etcd
                       github.com/miekg/dns
                       github.com/corpix/revip
                       github.com/pkg/errors)

               (var (DefaultConfig Config (create Config))
                    (DefaultListen string "0.0.0.0:5353")
                    (DefaultEtcdHostsConfig (slice string) (create (slice string) ("http://127.0.0.1:4001")))
                    (DefaultEtcdConfig (ptr EtcdConfig) (create (ref EtcdConfig)))

                    (Flags (slice cli.Flag)
                           (create (slice cli.Flag)
                                   ((create (ref cli.StringFlag)
                                            ((Name    "log-level")
                                             (Aliases (create (slice string) ("l")))
                                             (Usage   "logging level (debug, info, error)")
                                             (Value   "info")))
                                    (create (ref cli.StringFlag)
                                            ((Name    "config")
                                             (Aliases (create (slice string) ("c")))
                                             (EnvVars (create (slice string) ("DNS_CONFIG")))
                                             (Usage   "path to application configuration file")
                                             (Value   "dns.yaml"))))))
                    (Commands (slice (ptr cli.Command))
                              (create (slice (ptr cli.Command))
                                      ((create (ref cli.Command)
                                               ((Name    "config")
                                                (Aliases (create (slice string) ("c")))
                                                (Usage   "Configuration Tools")
                                                (Subcommands (create (slice (ptr cli.Command))
                                                                     ((create (ref cli.Command)
                                                                              ((Name    "show-default")
                                                                               (Aliases (create (slice string) ("sd")))
                                                                               (Usage   "Show default configuration")
                                                                               (Action   ConfigShowDefaultAction)))
                                                                      (create (ref cli.Command)
                                                                              ((Name    "show")
                                                                               (Aliases (create (slice string) ("s")))
                                                                               (Usage   "Show default configuration")
                                                                               (Action   ConfigShowAction))))))))))))

               ;;

               (alias yaml   (rename (NewYamlEncoder NewEncoder)))
               (alias errors (suffix Error  New Wrap Wrapf Cause))
               (alias regexp
                      (prefix Regexp Match Compile)
                      (type Regexp))

               ;;

               (func (Fatal ((err error)))
                     (fmt.Fprintf os.Stderr "fatal error: %s\n" err)
                     (os.Exit 1))

               ;;

               (type (SourceConfig (struct
                                     (Type string)
                                     (Etcd EtcdConfig)
                                     (File (struct (Path string)))
                                     (Static (map string string)))))

               ;;--

               (type (EtcdConfig (struct (Hosts (slice string)))))

               (func ((SetDefaults (c (ptr EtcdConfig))))
                     (when (or (== nil c.Hosts)
                               (== 0 (len c.Hosts)))
                       (set c.Hosts DefaultEtcdHostsConfig)))

               ;;--

               (type (Config (struct
                               (Listen   string)
                               (Sources (map string (ptr SourceConfig)))
                               (Routes  (slice string))
                               (Rewrite (slice string)))))

               (type (Provider (interface
                                 (Current (func))
                                 (Next (func)))))

               (type (Cond (struct
                             (Pred (ptr Regexp))
                             (Value string))))

               (type (Parser (interface
                                 (Parse (func ((xs (slice string))) (error))))))

               (type (Source (struct
                               (Client Provider)
                               (Parser Parser))))

               (func ((SetDefaults (c (ptr Config))))
                     (when (== ""  c.Listen) (set c.Listen DefaultListen))
                     (when (== nil c.Etcd)   (set c.Etcd   DefaultEtcdConfig))
                     (c.Etcd.SetDefaults))

               (func (LoadConfig ((path string)) ((c (ptr Config)) (err error)))
                     (set c (create (ref Config)))
                     (set err (ParseConfig c path))
                     (c.SetDefaults)
                     (return))

               (func (ParseConfig ((ptr (ptr Config)) (path string)) (error))
                     (def (fd err) ((os.Open path)))
                     (when (!= nil err) (return err))
                     (defer (fd.Close))
                     (set (_ err)
                          ((revip.Unmarshal ptr
                                            (revip.FromReader fd revip.YamlUnmarshaler)
                                            (revip.FromEnviron "dns"))))
                     (return err))

               ;;

               (func (ConfigShowDefaultAction ((ctx (ptr cli.Context))) (error))
                     (def enc (NewYamlEncoder os.Stdout))
                     (defer (enc.Close))
                     (DefaultConfig.SetDefaults)
                     (return (enc.Encode DefaultConfig)))

               (func (ConfigShowAction ((ctx (ptr cli.Context))) (error))
                     (def (c err) ((LoadConfig (ctx.String "config"))))
                     (when (!= nil err) (return err))
                     (def enc (NewYamlEncoder os.Stdout))
                     (defer (enc.Close))
                     (return (enc.Encode c)))

               (func (RootAction ((ctx (ptr cli.Context))) (error))
                     (def (c err) ((LoadConfig (ctx.String "config"))))
                     (when (!= nil err) (return err))
                     (return ((key (NewServer (NewClient c.Etcd.Hosts) c.Listen)
                                   Run))))

               ;;

               (func (NewApp () ((ptr cli.App)))
                     (def app (create (ref cli.App)))
                     (set app.Name "dns")
                     (set app.Flags Flags)
                     (set app.Action RootAction)
                     (set app.Commands Commands)
                     (return app))

               ;;

               (func (NewClient ((etcdHosts (slice string))) ((client (ptr etcd.Client))))
                     (set client (etcd.NewClient etcdHosts))
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
                       (when (== nil err) (s.Update n))
                       (log.Printf "ready for queries"))
                     (s.group.Wait)
                     (return nil))

               (func ((Stop (s (ptr Server))))
                     (send s.stop #t)
                     (s.group.Done)
                     (s.group.Done))

               (func ((Update (s (ptr Server))) ((e (ptr etcd.Response))))
                     (when (== nil e) (return))
                     ;; process the first and then loop over nodes
                     (def parts (strings.SplitN e.Node.Value "," 2))
                     (if (!= 2 (len parts))
                         (log.Printf "unable to parse node %s with value %s" e.Node.Key e.Node.Value)
                         (begin
                           (def err (s.router.Add (index parts 0) (index parts 1)))
                           (when (!= nil err) (log.Printf "unable to add %s" err))))
                     (for ((_ n) (range e.Node.Nodes))
                       (def parts (strings.SplitN n.Value "," 2))
                       (if (!= 2 (len parts))
                           (log.Printf "unable to parse node %s with value %s" n.Key n.Value)
                           (begin
                             (def err (s.router.Add (index parts 0) (index parts 1)))
                             (when (!= nil err) (log.Printf "unable to add %s" err))))))

               (func ((ServeDNS (s (ptr Server))) ((w dns.ResponseWriter) (req (ptr dns.Msg))))
                     (def q    (index (key req Question) 0))
                     (def name (strings.ToLower q.Name))
                     (when (or (== q.Qtype dns.TypeIXFR)
                               (== q.Qtype dns.TypeAXFR))
                       (def m (new dns.Msg))
                       (m.SetRcode req dns.RcodeServerFailure)
                       (w.WriteMsg m)
                       (return))

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

               ;; (func ((HealthCheck (s (ptr Server))))
               ;;       (def (c m) ((new dns.Client)
               ;;                   (new dns.Msg)))
               ;;       (set (c.Net m.Question)
               ;;            ("tcp" (make (type (slice dns.Question)) 1)))
               ;;       (set (index m.Question 0)
               ;;            (create dns.Question (DefaultHealthQuery dns.TypeTXT dns.ClassCHAOS)))
               ;;       (for ((_ serv) (range (s.router.Servers)))
               ;;         (when (or (not (check c m serv))
               ;;                   (not (check c m serv)))
               ;;           (log.Printf "healthcheck failed for %s" serv)
               ;;           (s.router.RemoveServer serv))))

               (func ((run (s (ptr Server))) ((mux (ptr dns.ServeMux)) (net  string)))
                     (defer (s.group.Done))
                     (def server (ref (create dns.Server
                                              ((Addr s.addr)
                                               (Net net)
                                               (Handler mux)
                                               (ReadTimeout s.readTimeout)
                                               (WriteTimeout s.writeTimeout)))))
                     (def err (server.ListenAndServe))
                     ;; we could use (server.ShutdownContext ctx) here
                     ;; but sould define some settings
                     (when (!= err nil) (Fatal err)))

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
                            (when (!= nil err) (return err))
                            (when (== nil (net.ParseIP ip))
                              (return (fmt.Errorf "not an IP address %s" dest))))
                     (begin (def (_ err) ((RegexpCompile re)))
                            (when (!= nil err) (return err))
                            (def (_ ok) ((index r.route re)))
                            (unless ok (set (index r.route re)
                                            (make (type (slice string)) 0)))
                            (for ((_ d) (range (index r.route re)))
                              (when (== d dest)
                                (log.Printf "address %s already in list for %s" dest re)
                                (return nil)))
                            (log.Printf "adding route %s for %s" re dest)
                            (set (index r.route re)
                                 (append (index r.route re) dest)))
                     (return nil))

               (func ((Remove (r (ptr Router)))
                      ((dest string) (re string)) (error))
                     (r.Lock)
                     (defer (r.Unlock))
                     (begin (def (_ err) ((RegexpCompile re)))
                            (when (!= nil err) (return err))
                            (def (_ ok) ((index r.route re)))
                            (unless ok (return (fmt.Errorf "Regexp %s does not exist" re)))
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
                           (when (!= nil err)
                             (log.Printf "%s" err))))))

               (func ((Match (r (ptr Router))) ((qname string)) ((slice string) error))
                     (r.RLock)
                     (defer (r.RUnlock))
                     (for ((re dest) (range r.route))
                       (def (ok _)
                         ((RegexpMatch re (cast qname (slice byte)))))
                       (when ok (return dest nil)))
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
                     (when (or (!= nil err)
                               (!= in.Rcode dns.RcodeSuccess))
                       (return false))
                     (return true))

               ;;

               (func (main)
                     (def err ((key (NewApp) Run) os.Args))
                     (when (!= nil err) (Fatal err))))
