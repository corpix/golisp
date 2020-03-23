#lang racket/base
(require racket/list
         racket/match)
(provide (all-defined-out))

(define *macro* (make-parameter (make-hasheq)))

(define (expand-macro ast)
  (let ((macro?
         (lambda (v)
           (and (list? v)
                (not (empty? v))
                (symbol? (car v))
                (hash-has-key? (*macro*) (car v))))))
    (match ast
      ((? struct? ast) ast)

      ((? macro? ast)
       (let* ((k (car ast))
              (a (cdr ast))
              (f (hash-ref (*macro*) k)))
         (apply f (if (list? a) a (list a)))))

      ((? list? ast) (map expand-macro ast))
      (_ ast))))
