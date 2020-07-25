#lang racket/base
(require racket/struct
         (except-in racket/list flatten)
         syntax/parse
         "syntax.rkt"
         "ast-go.rkt"
         "tool.rkt"
         "type.rkt"
         (for-syntax racket/base
                     syntax/parse))
(provide go/define-macro
         go/expand-macro
         go/expand
         go/string
         go/write-file
         *prelude*
         *epilogue*
         *scope*)

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define *prelude*                (make-parameter (box null)))
(define *epilogue*               (make-parameter (box null)))
(define *scope*                  (make-parameter (make-hasheq)))
(define *macro-expand-max-depth* (make-parameter 16))

(define (walk proc xs)
  (let ((recur  (partial walk proc))
        (result (proc xs)))
    (cond
      ((prefab-struct-key result)
       (apply make-prefab-struct
              (prefab-struct-key result)
              (map recur (struct->list result))))
      ((list? result)
       (map recur result))
      (#t result))))

;;

(define do-macro-define
  (partial walk
           (lambda (node)
             (begin0 node
               (when (go:macro? node)
                 (eval (quasiquote
                        (go/define-macro
                         ((unquote (go:macro-name node)) (unquote-splicing (go:macro-args node)))
                         (unquote-splicing (go:macro-expr node))))
                       ns))))))

(define do-macro-expand
  (partial walk
           (lambda (node)
             (cond ((go:func:call? node)
                    (let* ((proc-name (go:func:call-func node))
                           (proc (hash-ref (*scope*) proc-name #f)))
                      (cond (proc
                             (let* ((args (go:func:call-arguments node))
                                    (args-arity (length args)))
                               (unless (procedure-arity-includes? proc args-arity)
                                 (let ((proc-arity (procedure-arity proc)))
                                   (error (format "arity mismatch: macro '~a' expected ~a arguments, got ~a: ~a"
                                                  proc-name proc-arity args-arity node)))) ;; FIXME: convert node to code
                               (eval `(go/expand-syntax ,(apply proc args)) ns)))
                            (#t node))))
                   (#t node)))))

(define-syntax (go/define-macro stx)
  (syntax-parse stx
    ((_ (name:id args:id ...) body ...+)
     (syntax (hash-set!
              (*scope*) (quote name)
              (lambda (args ...) body ...))))))

;; TODO: make ast -> golisp converter
(define-syntax (go/expand-macro-once stx) ;; FIXME: should return gisp expr
  (syntax-parse stx
    ((_ xs:expr ...+)
     (syntax
      (parameterize ((*scope* (hash-copy (*scope*))))
          (do-macro-expand (do-macro-define (go/expand-syntax xs ...))))))))

(define-syntax (go/expand-macro stx)
  (syntax-parse stx
    ((_ xs:expr ...+)
     (syntax
      (let loop ((counter 0)
                 (curr    (go/expand-macro-once xs ...))
                 (prev    null))
        (cond
          ((>= counter (*macro-expand-max-depth*))
           (error (format "macro expansion depth limit reached: limit ~a" counter)))
          ((not (equal? prev curr))
           (loop (add1 counter)
                 (eval `(go/expand-macro-once ,@curr) ns)
                 curr))
          (#t curr)))))))

(define-syntax (go/expand stx)
  (syntax-parse stx
    ((_ xs:expr ...+)
     (quasisyntax
      (parameterize ((*prelude*  (box null))
                     (*epilogue* (box null)))
        (let* ((ast      (go/expand-macro xs ...))
               (prelude  (unbox (*prelude*)))
               (epilogue (unbox (*epilogue*))))
          (when (not (empty? prelude))
            (set! ast (cons (car ast) (append prelude (cdr ast)))))
          (when (not (empty? epilogue))
            (set! ast (append ast (unbox (*epilogue*)))))
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

(define-gosyntax (prelude stx)
  (syntax-parse stx
    ((_ xs:ExprRecur ...+)
     (quasisyntax
      (set-box! (*prelude*)
                (append (unbox (*prelude*))
                        (go/expand-macro (unsyntax-splicing (attribute xs.ast)))))))))

(define-gosyntax (epilogue stx)
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
                (parameterize ((*scope* (make-hasheq)))
                  (go/define-macro
                   (foo a b)
                   (quasiquote (+ (unquote a) (unquote b))))
                  (check-equal? (go/expand-macro (foo 1 2))
                                (quote (+ 1 2)))
                  (check-equal? (go/expand-macro (bar (foo 1 2)))
                                (quote (bar (+ 1 2))))
                  (check-equal? (go/expand-macro (list (foo 1 2) (foo 1 2)))
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
