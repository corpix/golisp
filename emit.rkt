#lang racket/base
(require racket/match
         racket/string
         racket/format
         racket/list

         "type.rkt"
         "tool.rkt")

(provide go/emit)

;;

(define +empty+    "")
(define +new-line+ "\n")
(define +tab+      "\t")
(define +comma+    ",")
(define +eq+       "=")
(define +space+    " ")
(define +lbracket+ "(")
(define +rbracket+ ")")

;;

(define (emit-operator ast)
  (match ast
    ((go:operator id operands)
     (string-append
      +lbracket+
      (string-join
       (map emit-expr operands)
       (string-append +space+ (symbol->string id) +space+))
      +rbracket+))))

(define (emit-package ast)
  (match ast
    ((go:package name) (string-append "package" +space+ (symbol->string name)))))

(define (emit-imports ast)
  (match ast
    ((go:imports imports)
     (string-append "import" +space+
                    +lbracket+
                    +new-line+
                    (string-join (map emit-imports imports) +new-line+)
                    +new-line+
                    +rbracket+))
    ((go:import package altname)
     (string-append
      +tab+
      (if altname
          (string-append (*->string altname) +space+)
          +empty+)
      (*->string package)))))

(define (emit-var ast)
  (match ast
    ((go:var bindings)
     (string-append "var" +space+ (emit-var bindings)))
    ((and (list ast ...) (list (? go:var:binding?) ...))
     (string-append +lbracket+
                    (string-join (map emit-var ast)
                                 (string-append +comma+ +space+))
                    +rbracket+))
    ((go:var:binding name type value)
     (string-append
      (symbol->string name) +space+ (symbol->string type)
      (or (and value
               (string-append +eq+
                              (emit-expr value)))
          +empty+)))))

;; (define (emit-func ast)
;;   (match ast
;;     ((go:func name i o body)
;;      (string-append "func" +space+
;;                     (*->string name)       (emit-var-bindings (or i null))
;;                     +space+                (emit-var-bindings (or o null))
;;                     +space+ "{" +new-line+ (emit-expr body) +new-line+ "}"))))



(define (emit-tuple ast)
  (let ((concat
         (lambda (v)
           (format "~a ~a"
                   (car v)
                   (cdr v)))))
    (match ast
      ((and (list ast ...) (list (? pair?) ...))
       (string-append
        "(" (string-join (map concat ast) +comma+) ")"))
      ((and (list ast ...) (list (? symbol?) ...))
       (string-append
        "(" (string-join (map symbol->string ast) +comma+) ")")))))

(define (emit-id ast)     (~a ast))
(define (emit-string ast) (~s ast))
(define (emit-number ast) (~a ast))

(define (emit-expr ast)
  (match ast
    ((go:expr xs) (emit-expr xs))

    ((? go:operator? ast) (emit-operator ast))
    ((? go:package?  ast) (emit-package  ast))
    ((? go:imports?  ast) (emit-imports  ast))
    ;;((? go:func?    ast) (emit-func     ast))
    ((? go:expr?     ast) (emit-expr     ast))
    ((? go:var?      ast) (emit-var      ast))

    ;; FIXME: make it more strict by not supporting this? (problematic if we want it to be recursive)
    ((? symbol? ast) (emit-id (symbol->string ast)))
    ((? string? ast) (emit-string ast))
    ((? number? ast) (emit-number ast))

    ((? (lambda (v) (and (list? v)
                         (not (empty? v))
                         (symbol? (car v))))
        ast)
     (format "~a(~a)"
             (car ast)
             (string-join (map emit-expr (cdr ast))
                          +comma+)))

    ((? list? ast)
     (string-join
      (map emit-expr ast)
      +new-line+))))

(define go/emit emit-expr)


(module+ test
  (require rackunit
           rackunit/text-ui)

  (for
      ((suite (list
               (test-suite "operator"
                           (for ((operator (quote (+ - % * / == != > < >= <=
                                                     ! && ||
                                                     &  \| ^ << >>))))
                             (check-equal? (emit-operator (go:operator operator
                                                                       (list
                                                                        (go:expr 1)
                                                                        (go:expr 2))))
                                           (format "(1 ~a 2)" operator))
                             (check-equal? (emit-operator (go:operator operator
                                                                       (list
                                                                        (go:expr 1)
                                                                        (go:expr 2)
                                                                        (go:expr 3))))
                                           (format "(1 ~a 2 ~a 3)" operator operator))
                             (check-equal? (emit-operator (go:operator operator
                                                                       (list
                                                                        (go:expr 1)
                                                                        (go:expr 2)
                                                                        (go:expr (go:operator operator
                                                                                              (list
                                                                                               (go:expr 1)
                                                                                               (go:expr 2)
                                                                                               (go:expr 3)))))))
                                           (format "(1 ~a 2 ~a (1 ~a 2 ~a 3))"
                                                   operator operator operator operator))))
               (test-suite "package"
                           (check-equal? (emit-package (go:package 'foo))
                                         "package foo")
                           (check-equal? (emit-package (go:package 'bar))
                                         "package bar"))
               (test-suite "imports"
                           (check-equal? (emit-imports (go:imports
                                                        (list
                                                         (go:import 'foo #f)
                                                         (go:import 'bar #f))))
                                         (string-append "import" +space+
                                                        +lbracket+
                                                        +new-line+ +tab+ "foo"
                                                        +new-line+ +tab+ "bar"
                                                        +new-line+
                                                        +rbracket+))
                           (check-equal? (emit-imports (go:imports
                                                        (list
                                                         (go:import 'foo #f)
                                                         (go:import 'bar 'baz))))
                                         (string-append "import" +space+
                                                        +lbracket+
                                                        +new-line+ +tab+ "foo"
                                                        +new-line+ +tab+ "baz" +space+ "bar"
                                                        +new-line+
                                                        +rbracket+)))
               )))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
