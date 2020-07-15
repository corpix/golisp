#lang racket

'(1 2 3)
(list 1 2 3)
(vector 1 2 3)
(vector-ref (vector 1 2 3) 9)

(list 1 2 3)
(cons 1 (cons 2 (cons 3 (list))))

(quote (cons 1 (cons 2 (cons 3 (list)))))

'test
(quote test)
(quasiquote (cons 1 (cons 2 (cons (unquote (+ 1 3)) (list)))))

(define name 'value)
name

(let ((x 1) (y 2))
  (+ x y))

(define name 'value)

(define (plus a b)
  (+ a b))

(plus 2 2)

(define-syntax (my-if stx)
  (syntax-case stx ()
    ((_ cond then else)
     (syntax (if cond then else)))))

(my-if #t (displayln 1) (displayln 2))
