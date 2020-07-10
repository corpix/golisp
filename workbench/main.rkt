#lang racket/base
(require "../main.rkt")

(go/write-file "main.go"
               (package main)
               (import os fmt regexp time context

                       (cli  github.com/urfave/cli/v2)
                       (yaml gopkg.in/yaml.v2)
                       (etcd github.com/coreos/etcd/clientv3)
                       github.com/corpix/revip
                       github.com/pkg/errors)

               ;;

               (alias (yaml (rename (NewYamlEncoder NewEncoder)))
                      (errors (suffix Error
                                      New Cause
                                      Wrap Wrapf))
                      (regexp (prefix Regexp Match Compile)
                              (type Regexp))
                      (os Stdout Stderr Args Open Exit)
                      (fmt Fprintf Errorf)
                      (etcd (prefix Etcd
                                    (rename (Connect New))
                                    (type Config)))
                      (revip YamlUnmarshaler
                             (prefix Config
                                     Unmarshal
                                     FromReader FromEnviron)))

               (func (fatal ((err error)))
                     (Fprintf Stderr "fatal error: %s\n" err)
                     (Exit 1))

               ;;

               (const (EnvironPrefix string "DNS"))

               ;;

               (type (Source (interface
                               (Watch (func (((key string))
                                             ((values (chan (slice byte)))
                                              (err error)))))
                               (Get (func (((key string))
                                           ((value (slice byte))
                                            (err error)))))
                               (Set (func (((key string) (value (slice byte)))
                                           ((err error))))))))

               ;; (type (EtcdSource (struct
               ;;                     ())))

               ;; (type (SourceConfig (struct
               ;;                       (Type string)
               ;;                       (Etcd EtcdConfig)
               ;;                       (File (struct (Path string)))
               ;;                       (Static (map string string)))))

               (type (Config (struct
                               (Listen string))))
               (func ((SetDefaults (c (ptr Config))))
                     (when (== ""  c.Listen) (set c.Listen DefaultListen)))

               ;;


               (func (LoadConfig ((path string)) ((c (ptr Config)) (err error)))
                     (set c (create (ref Config)))
                     (set err (ParseConfig c path))
                     (c.SetDefaults)
                     (return))

               (func (ParseConfig ((ptr (ptr Config)) (path string)) (error))
                     (def (fd err) ((Open path)))
                     (when (!= nil err) (return err))
                     (defer (fd.Close))
                     (set (_ err)
                          ((ConfigUnmarshal ptr
                                            (ConfigFromReader fd YamlUnmarshaler)
                                            (ConfigFromEnviron EnvironPrefix))))
                     (return err))

               ;;

               (func (ConfigShowDefaultAction ((ctx (ptr cli.Context))) (error))
                     (def enc (NewYamlEncoder Stdout))
                     (defer (enc.Close))
                     (DefaultConfig.SetDefaults)
                     (return (enc.Encode DefaultConfig)))

               (func (ConfigShowAction ((ctx (ptr cli.Context))) (error))
                     (def (c err) ((LoadConfig (ctx.String "config"))))
                     (when (!= nil err) (return err))
                     (def enc (NewYamlEncoder Stdout))
                     (defer (enc.Close))
                     (return (enc.Encode c)))

               (func (RootAction ((ctx (ptr cli.Context))) (error))
                     (def (_ err) ((LoadConfig (ctx.String "config"))))
                     (when (!= nil err) (return err))
                     (return nil))

               ;;

               (var (DefaultConfig Config (create Config))
                    (DefaultListen string "0.0.0.0:5353")

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
                                             (EnvVars (create (slice string) ((+ EnvironPrefix "_CONFIG"))))
                                             (Usage   "path to application configuration file")
                                             (Value   "config.yaml"))))))
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

               (func (NewApp () ((ptr cli.App)))
                     (def app (create (ref cli.App)))
                     (set app.Flags Flags)
                     (set app.Action RootAction)
                     (set app.Commands Commands)
                     (return app))

               ;;

               (func (main)
                     (def err ((key (NewApp) Run) Args))
                     (when (!= nil err) (fatal err))

                     (def (cli err) ((EtcdConnect
                                      (create EtcdConfig
                                              ((Endpoints (create (slice string) ("127.0.0.1:2379")))
                                               (DialTimeout (* 5 (key time Second))))))))
                     (when (!= nil err) (fatal err))

                     (def (res err) (((key cli KV Put)
                                      ((key context Background))
                                      "hello" "world")))
                     (when (!= nil err) (fatal err))
                     (fmt.Println res)

                     (defer ((key cli Close)))

                     ))
