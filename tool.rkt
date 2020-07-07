#lang racket/base
(require racket/match
         racket/bool
         racket/list)
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define (*->symbol v)
  (match v
    ((? symbol?  v) v)
    ((? number?  v) v)
    ((? string?  v) (string->symbol v))
    ((? keyword? v) (*->symbol (keyword->string v)))
    ((? syntax?  v) (*->symbol (syntax->datum v)))))

(define (*->string v)
  (match v
    ((? symbol?  v) (symbol->string v))
    ((? string?  v)  v)
    ((? number?  v) (number->string v))
    ((? false?   v)  "")))

(define ((partial f . xs) . xxs)
  (apply f (append xs xxs)))

(define (insert-at l p v)
  (define-values (before after) (split-at l p))
  (append before (cons v after)))

  (define (flatten xs)
    (if (list? xs)
        (for/fold ((acc null))
                  ((x (in-list xs)))
          (set! acc (append acc
                            (if (list? x)
                                (flatten x)
                                (list x))))
          acc)
        xs))

(define (hash-set-cons! hash key v)
  (hash-set! hash key (cons v (hash-ref hash key null))))

(module+ test
  (require rackunit)

  (test-case "partial"
    (define (+* x y) (+ x y))
    (check-equal? ((partial +*) 1 2) 3)
    (check-equal? ((partial +* 1) 2) 3)
    (check-equal? ((partial +* 1 2)) 3))

  (test-case "flatten"
    (check-equal?
     (flatten (list (cons 1 2) (list 3)
                    (list (list (list (cons 1 2))))))
     '((1 . 2) 3 (1 . 2))))

  (test-case "hash-set-cons!"
    (check-equal?
     (let ((h (make-hasheq)))
       (hash-set-cons! h 'foo 1)
       h)
     (make-hasheq (list (cons 'foo (list 1)))))
    (check-equal?
     (let ((h (make-hasheq)))
       (hash-set-cons! h 'foo 1)
       (hash-set-cons! h 'foo 2)
       h)
     (make-hasheq (list (cons 'foo (list 2 1)))))))
