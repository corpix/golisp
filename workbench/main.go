package main

import (
	"fmt"
	"github.com/coreos/go-etcd/etcd"
	"github.com/corpix/revip"
	"github.com/miekg/dns"
	cli "github/urfave/cli/v2"
	"log"
	"net"
	"regexp"
	"strings"
	"sync"
	"time"
)

var (
	DefaultEtcdHosts   []string   = []string{"http://127.0.0.1:4001"}
	DefaultAddr        string     = "0.0.0.0:5353"
	DefaultHealthQuery string     = "id.server."
	Flags              []cli.Flag = []cli.Flag(&cli.StringFlag{Name: "log-level", Aliases: []string{"l"}, Usage: "logging level (debug, info, error)", Value: "info"})
)

func NewClient() (client *etcd.Client) {
	client = etcd.NewClient(DefaultEtcdHosts)
	client.SyncCluster()
	return client
}

type Server struct {
	client       *etcd.Client
	addr         string
	readTimeout  time.Duration
	writeTimeout time.Duration
	group        *sync.WaitGroup
	router       *Router
	stop         chan bool
}

func NewServer(client *etcd.Client, addr string) (server *Server) {
	if addr == "" {
		addr = DefaultAddr
	}
	return &Server{client: client, addr: addr, group: new(sync.WaitGroup), router: NewRouter(), stop: make(chan bool)}
}
func (s *Server) Run() error {
	mux := dns.NewServeMux()
	mux.Handle(".", s)
	s.group.Add(2)
	go s.run(mux, "tcp")
	go s.run(mux, "udp")
	{
		log.Printf("enabling health checking")
		go func() {
			for {
				time.Sleep((5 * 1000000000.0))
				s.HealthCheck()
			}
		}()
	}
	{
		log.Printf("setting watch")
		ch := make(chan *etcd.Response)
		go func() {
			go s.client.Watch("/dnsrouter", 0, true, ch, s.stop)
			for {
				select {
				case n := <-ch:
					s.Update(n)
				}
			}
		}()
	}
	{
		log.Printf("getting initial list")
		n, err := s.client.Get("/dnsrouter/", false, true)
		if err != nil {
			s.Update(n)
		}
		log.Printf("ready for queries")
	}
	s.group.Wait()
	return nil
}
func (s *Server) Stop() {
	s.stop <- true
	s.group.Done()
	s.group.Done()
}
func (s *Server) Update(e *etcd.Response) {
	if nil == e {
		return
	}
	parts := strings.SplitN(e.Node.Value, ",", 2)
	if 2 != len(parts) {
		log.Printf("unable to parse node %s with value %s", e.Node.Key, e.Node.Value)
	} else {
		err := s.router.Add(parts[0], parts[1])
		if nil != err {
			log.Printf("unable to add %s", err)
		}
	}
	for _, n := range e.Node.Nodes {
		parts := strings.SplitN(n.Value, ",", 2)
		if 2 != len(parts) {
			log.Printf("unable to parse node %s with value %s", n.Key, n.Value)
		} else {
			err := s.router.Add(parts[0], parts[1])
			if nil != err {
				log.Printf("unable to add %s", err)
			}
		}
	}
}
func (s *Server) ServeDNS(w dns.ResponseWriter, req *dns.Msg) {
	q := req.Question[0]
	name := strings.ToLower(q.Name)
	if (q.Qtype == dns.TypeIXFR) || (q.Qtype == dns.TypeAXFR) {
		m := new(dns.Msg)
		m.SetRcode(req, dns.RcodeServerFailure)
		w.WriteMsg(m)
		return
	}
	servers, err := s.router.Match(name)
	if (nil != err) || (0 == len(servers)) {
		m := new(dns.Msg)
		m.SetRcode(req, dns.RcodeServerFailure)
		w.WriteMsg(m)
		return
	}
	serv := servers[(int(dns.Id()) % len(servers))]
	log.Printf("routing %s to %s", name, serv)
	c := new(dns.Client)
	ret, _, err := c.Exchange(req, serv)
	if nil != err {
		m := new(dns.Msg)
		m.SetRcode(req, dns.RcodeServerFailure)
		w.WriteMsg(m)
		return
	}
	w.WriteMsg(ret)
}
func (s *Server) HealthCheck() {
	c, m := new(dns.Client), new(dns.Msg)
	c.Net, m.Question = "tcp", make([]dns.Question, 1)
	m.Question[0] = dns.Question(DefaultHealthQuery(dns.TypeTXT, dns.ClassCHAOS))
	for _, serv := range s.router.Servers() {
		if (!check(c, m, serv)) || (!check(c, m, serv)) {
			log.Printf("healthcheck failed for %s", serv)
			s.router.RemoveServer(serv)
		}
	}
}
func (s *Server) run(mux *dns.ServeMux, net string) {
	defer s.group.Done()
	server := &dns.Server{Addr: s.addr, Net: net, Handler: mux, ReadTimeout: s.readTimeout, WriteTimeout: s.writeTimeout}
	err := server.ListenAndServe()
	if err != nil {
		log.Fatal(err)
	}
}

type Router struct {
	sync.RWMutex
	route map[string][]string
}

func NewRouter() *Router {
	r := make(map[string][]string)
	return &Router{route: r}
}
func (r *Router) Add(dest string, re string) error {
	r.Lock()
	defer r.Unlock()
	{
		ip, _, err := net.SplitHostPort(dest)
		if nil != err {
			return err
		}
		if nil == net.ParseIP(ip) {
			return fmt.Errorf("not an IP address %s", dest)
		}
	}
	{
		_, err := regexp.Compile(re)
		if nil != err {
			return err
		}
		_, ok := r.route[re]
		if !ok {
			r.route[re] = make([]string, 0)
		}
		for _, d := range r.route[re] {
			if d == dest {
				log.Printf("address %s already in list for %s", dest, re)
				return nil
			}
		}
		log.Printf("adding route %s for %s", re, dest)
		r.route[re] = append(r.route[re], dest)
	}
	return nil
}
func (r *Router) Remove(dest string, re string) error {
	r.Lock()
	defer r.Unlock()
	{
		_, err := regexp.Compile(re)
		if nil != err {
			return err
		}
		_, ok := r.route[re]
		if !ok {
			return fmt.Errorf("Regexp %s does not exist", re)
		}
		for i, s := range r.route[re] {
			if s == dest {
				log.Printf("removing %s", s)
				r.route[re] = append(r.route[re][0:i], r.route[re][(1+i):]...)
				return nil
			}
		}
	}
	return nil
}
func (r *Router) RemoveServer(serv string) {
	for rec, servs := range r.route {
		for _, serv1 := range servs {
			if serv1 == serv {
				err := r.Remove(serv, rec)
				if nil != err {
					log.Printf("%s", err)
				}
			}
		}
	}
}
func (r *Router) Match(qname string) ([]string, error) {
	r.RLock()
	defer r.RUnlock()
	for re, dest := range r.route {
		ok, _ := regexp.Match(re, []byte(qname))
		if ok {
			return dest, nil
		}
	}
	return nil, fmt.Errorf("No match for %s", qname)
}
func (r *Router) Servers() []string {
	r.RLock()
	defer r.RUnlock()
	s := make([]string, 0, 5)
	for _, dest := range r.route {
		s = append(s, dest...)
	}
	return s
}
func check(c *dns.Client, m *dns.Msg, addr string) bool {
	m.Id = dns.Id()
	in, _, err := c.Exchange(m, addr)
	if nil != err {
		return false
	}
	if in.Rcode != dns.RcodeSuccess {
		return false
	}
	return true
}
func main() {
	s := NewServer(NewClient(), DefaultAddr)
	err := s.Run()
	if err != nil {
		log.Fatal(err)
	}
}
