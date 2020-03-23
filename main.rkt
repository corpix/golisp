#lang racket/base
(require syntax/parse
         racket/string
         racket/match
         racket/list
         "syntax.rkt"
         (for-syntax racket/base
                     syntax/parse
                     "syntax.rkt"))
(provide #%app)

;; (displayln
;;  (go->string (package main)
;;              (prelude (var ((y bool))))
;;              (import "net/http"
;;                      (f "fmt"))
;;              (func x ((a string) (b int)) ((x error))
;;                    (var ((x int 1)))
;;                    (+ x x))))

;;  (package main)
;;  (import
;;   ;; XXX: idea - common hostname grouping support?
;;   ;; (maybe via dynamic variable flag)
;;   "fmt"
;;   "os"
;;   "gopkg.in/urfave/cli.v2")
;;  (var ((flags
;;         (slice-of (. cli Flag))
;;         (make (slice-of (. cli Flag))))
;;        (command
;;         (slice-of (ptr (. cli Command)))
;;         (make (slice-of (ptr (. cli Command)))))))
;;  (func rootAction ((ctx (ptr (. cli Context)))) (error)
;;        (fmt.Println "hello")
;;        (return nil))
;;  (func main () ()
;;        (def app (new (. cli App)))
;;        (set (. app Name) "hello")
;;        (set (. app Action) rootAction)
;;        ((. app Run) (. os Args)))
