#lang racket/base
(require racket/struct
         (except-in racket/list flatten)
         syntax/parse
         "syntax.rkt"
         "ast-go.rkt"
         "tool.rkt"
         "type.rkt"
         "parameter.rkt"
         "transformer.rkt"
         (for-syntax racket/base
                     syntax/parse))
(provide go/expand-macro-once
         go/expand-macro
         go/expand
         go/string
         go/write-file)

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define-syntax (go/expand-macro-once stx)
  (syntax-parse stx
    ((_ xs:expr ...+)
     (syntax
      (go/with-env ((*transformers* (list (make-macro-transformer))))
                   (go/transform-once xs ...))))))

(define-syntax (go/expand-macro stx)
  (syntax-parse stx
    ((_ xs:expr ...+)
     (syntax
      (go/with-env ((*transformers* (list (make-macro-transformer))))
                   (go/transform xs ...))))))

(define-syntax (go/expand stx)
  (syntax-parse stx
    ((_ xs:expr ...+)
     (quasisyntax
      (go/with-env ()
                   (let* ((ast      (go/transform xs ...))
                          (prelude  (unbox (*prelude*)))
                          (epilogue (unbox (*epilogue*))))
                     (when (not (empty? prelude))
                       (set! ast (cons (car ast) (append prelude (cdr ast)))))
                     (when (not (empty? epilogue))
                       (set! ast (append ast epilogue)))
                     ast))))))

(define-syntax (go/string stx)
  (syntax-parse stx
    ((_ xs ...)
     (syntax (ast->go (go/expand xs ...))))))

(define-syntax (go/write-file stx)
  (syntax-parse stx
    ((_ (~or* name:string name:id) exprs:expr ...)
     (syntax (with-output-to-file #:exists (quote replace)
               name
               (lambda () (display (go/string exprs ...))))))))

;;

(go/define-special (prelude stx)
  (syntax-parse stx
    ((_ xs:ExprRecur ...+)
     (quasisyntax
      (set-box! (*prelude*)
                (append (unbox (*prelude*))
                        (go/expand-macro (unsyntax-splicing (attribute xs.ast)))))))))

(go/define-special (epilogue stx)
  (syntax-parse stx
    ((_ xs:ExprRecur ...+)
     (quasisyntax
      (set-box! (*epilogue*)
                (append (unbox (*epilogue*))
                        (go/expand-macro (unsyntax-splicing (attribute xs.ast)))))))))

(module+ test
  (require rackunit)

  (run-suites
   (list
    (test-suite "go/expand-macro"
                (go/with-env ()
                             (go/define-macro
                              (foo a b)
                              (quasiquote (+ (unquote a) (unquote b))))
                             (check-equal? (go/expand-macro (foo 1 2))
                                           (quote (+ 1 2)))
                             (check-equal? (go/expand-macro (bar (foo 1 2)))
                                           (quote (bar (+ 1 2))))
                             (check-equal? (go/expand-macro (foo 1 2) (foo 1 2))
                                           (quote ((+ 1 2) (+ 1 2))))
                             (check-equal? (exn-message (with-handlers ((exn? (lambda (e) e)))
                                                          (go/expand-macro (foo))))
                                           "arity mismatch: macro 'foo' expected 2 arguments, got 0: (foo)")
                             (check-equal? (go/expand-macro (list (bar 1 2)
                                                                  (macro (bar (x y))
                                                                    (quasiquote (+ (unquote x)
                                                                                   (unquote y))))))
                                           (quote ((+ 1 2)
                                                   (macro (bar (x y))
                                                     (quasiquote (+ (unquote x)
                                                                    (unquote y)))))))))
    (test-suite "go/expand"
                (check-equal? (go/expand (macro (foo (x)) (quasiquote (+ 1 (unquote x))))
                                         (foo 2))
                              null)))))
