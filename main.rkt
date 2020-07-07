#lang racket/base
(require syntax/parse
         racket/string
         racket/match
         racket/list
         syntax/parse
         "syntax.rkt"
         "emit.rkt"
         (for-syntax racket/base
                     syntax/parse
                     "syntax.rkt"))
(provide (all-defined-out)
         go/expand
         go/emit)

(define-syntax-rule (go/string exprs ...)
  (go/emit (go/expand exprs ...)))

(define-syntax (go/write-file stx)
  (syntax-parse stx
    ((_ (~or* name:string name:id) exprs:expr ...)
     (syntax (with-output-to-file #:exists (quote replace)
               name
               (lambda () (display (go/string exprs ...))))))))
