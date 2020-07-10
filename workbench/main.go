package main

import (
	"context"
	"fmt"
	etcd "github.com/coreos/etcd/clientv3"
	"github.com/corpix/revip"
	"github.com/pkg/errors"
	cli "github.com/urfave/cli/v2"
	yaml "gopkg.in/yaml.v2"
	"os"
	"regexp"
	"time"
)

var (
	NewYamlEncoder = yaml.NewEncoder
)
var (
	NewError   = errors.New
	CauseError = errors.Cause
	WrapError  = errors.Wrap
	WrapfError = errors.Wrapf
)
var (
	RegexpMatch   = regexp.Match
	RegexpCompile = regexp.Compile
)

type (
	Regexp = regexp.Regexp
)

var (
	Stdout = os.Stdout
	Stderr = os.Stderr
	Args   = os.Args
	Open   = os.Open
	Exit   = os.Exit
)
var (
	Fprintf = fmt.Fprintf
	Errorf  = fmt.Errorf
)
var (
	EtcdConnect = etcd.New
)

type (
	EtcdConfig = etcd.Config
)

var (
	YamlUnmarshaler   = revip.YamlUnmarshaler
	ConfigUnmarshal   = revip.Unmarshal
	ConfigFromReader  = revip.FromReader
	ConfigFromEnviron = revip.FromEnviron
)

func fatal(err error) {
	Fprintf(Stderr, "fatal error: %s\n", err)
	Exit(1)
}

const (
	EnvironPrefix string = "DNS"
)

type Source interface {
	Watch(key string) (values chan []byte, err error)
	Get(key string) (value []byte, err error)
	Set(key string, value []byte) (err error)
}
type Config struct {
	Listen string
}

func (c *Config) SetDefaults() {
	if "" == c.Listen {
		c.Listen = DefaultListen
	}
}
func LoadConfig(path string) (c *Config, err error) {
	c = &Config{}
	err = ParseConfig(c, path)
	c.SetDefaults()
	return
}
func ParseConfig(ptr *Config, path string) error {
	fd, err := Open(path)
	if nil != err {
		return err
	}
	defer fd.Close()
	_, err = ConfigUnmarshal(ptr, ConfigFromReader(fd, YamlUnmarshaler), ConfigFromEnviron(EnvironPrefix))
	return err
}
func ConfigShowDefaultAction(ctx *cli.Context) error {
	enc := NewYamlEncoder(Stdout)
	defer enc.Close()
	DefaultConfig.SetDefaults()
	return enc.Encode(DefaultConfig)
}
func ConfigShowAction(ctx *cli.Context) error {
	c, err := LoadConfig(ctx.String("config"))
	if nil != err {
		return err
	}
	enc := NewYamlEncoder(Stdout)
	defer enc.Close()
	return enc.Encode(c)
}
func RootAction(ctx *cli.Context) error {
	_, err := LoadConfig(ctx.String("config"))
	if nil != err {
		return err
	}
	return nil
}

var (
	DefaultConfig Config     = Config{}
	DefaultListen string     = "0.0.0.0:5353"
	Flags         []cli.Flag = []cli.Flag{&cli.StringFlag{Name: "log-level",
		Aliases: []string{"l"},
		Usage:   "logging level (debug, info, error)",
		Value:   "info"},
		&cli.StringFlag{Name: "config",
			Aliases: []string{"c"},
			EnvVars: []string{(EnvironPrefix + "_CONFIG")},
			Usage:   "path to application configuration file",
			Value:   "config.yaml"}}
	Commands []*cli.Command = []*cli.Command{&cli.Command{Name: "config",
		Aliases: []string{"c"},
		Usage:   "Configuration Tools",
		Subcommands: []*cli.Command{&cli.Command{Name: "show-default",
			Aliases: []string{"sd"},
			Usage:   "Show default configuration",
			Action:  ConfigShowDefaultAction},
			&cli.Command{Name: "show",
				Aliases: []string{"s"},
				Usage:   "Show default configuration",
				Action:  ConfigShowAction}}}}
)

func NewApp() *cli.App {
	app := &cli.App{}
	app.Flags = Flags
	app.Action = RootAction
	app.Commands = Commands
	return app
}
func main() {
	err := NewApp().Run(Args)
	if nil != err {
		fatal(err)
	}
	cli, err := EtcdConnect(EtcdConfig{Endpoints: []string{"127.0.0.1:2379"},
		DialTimeout: (5 * time.Second)})
	if nil != err {
		fatal(err)
	}
	res, err := cli.KV.Put(context.Background(), "hello", "world")
	if nil != err {
		fatal(err)
	}
	fmt.Println(res)
	defer cli.Close()
}
