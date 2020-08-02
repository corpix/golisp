#lang racket/base
(require syntax/parse
         "syntax.rkt"
         "type.rkt"
         "parameter.rkt"
         (for-syntax racket/base
                     syntax/parse))
(provide make-macro-transformer
         go/define-macro)

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (do-define node)
  (begin0 node
    (when (go:macro? node)
      (eval `(go/define-macro
               (,(go:macro-name node) ,@(go:macro-args node))
               ,@(go:macro-expr node))
            ns))))

(define (do-expand node)
  (cond ((go:func:call? node)
         (let* ((proc-name (go:func:call-func node))
                (proc (hash-ref (*macro*) proc-name #f)))
           (cond (proc
                  (let* ((args (go:func:call-arguments node))
                         (args-arity (length args)))
                    (unless (procedure-arity-includes? proc args-arity)
                      (let ((proc-arity (procedure-arity proc)))
                        (error (format "arity mismatch: macro '~a' expected ~a arguments, got ~a: ~a"
                                       proc-name proc-arity args-arity node))))
                    ;; FIXME: convert node to code
                    ;;        or better add meta data into ast
                    ;;       (will help with sourcemaps, but requires parser changes)
                    (let* ((expr   (apply proc args))
                           (result (eval `(go/expand-syntax ,expr) ns)))
                      (walk:skip (go:expr-exprs (car result))))))
                 (#t node))))
        (#t node)))

(define (make-macro-transformer)
  (transformer 'macro
               (map proc
                    `(define expand)
                    `(,do-define ,do-expand))))

;;

(define-syntax (go/define-macro stx)
  (syntax-parse stx
    ((_ (name:id args:id ...) body ...+)
     (syntax (hash-set!
              (*macro*)
              (quote name)
              (lambda (args ...) body ...))))))

(module+ test
  (require rackunit
           "tool.rkt")
  (run-suites
   (list (test-suite "go/define-macro"
                     (parameterize ((*macro* (make-hasheq)))
                       (go/define-macro
                         (foo a b)
                         (quasiquote (+ (unquote a) (unquote b))))
                       (check-equal? (procedure? (hash-ref (*macro*) 'foo #f))
                                     #t))))))
