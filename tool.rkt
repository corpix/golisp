#lang racket/base
(require racket/match
         racket/bool
         racket/list)
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define (*->symbol v)
  (match v
    ((? symbol?  v) v)
    ((? string?  v) (string->symbol v))
    ((? keyword? v) (*->symbol (keyword->string v)))
    ((? syntax?  v) (*->symbol (syntax->datum v)))))

(define (*->string v)
  (match v
    ((? symbol?  v) (symbol->string v))
    ((? string?  v)  v)
    ((? false?   v) "")))

(define ((partial f . xs) . xxs)
  (apply f (append xs xxs)))

(define (insert-at l p v)
  (define-values (before after) (split-at l p))
  (append before (cons v after)))

(module+ test
  (require rackunit)

  (define (+* x y) (+ x y))
  (check-equal? ((partial +*) 1 2) 3)
  (check-equal? ((partial +* 1) 2) 3)
  (check-equal? ((partial +* 1 2)) 3))
