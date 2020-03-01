#lang racket/base
(require syntax/parse
         racket/string
         racket/match
         "syntax.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide #%app)

;;

(define-syntax-rule (go->string . stmts) (emit (expand (quote stmts))))

(display
 (go->string (package main)
             (import "fmt" (e "errors"))
             (func (say ((msg string) (who int)) ((r bool)))
                   (fmt.Println msg who)
                   (return true))
             (func (main () ())
                   (fmt.Println "chingyang!"
                                "mother" "fucker!"))
             ;;anonymous
             (func () (fmt.Println "aaaaa"))
             )
 (current-output-port))

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
