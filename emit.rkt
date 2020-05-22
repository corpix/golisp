#lang racket/base
(require racket/match
         racket/string
         racket/format
         racket/list

         "type.rkt"
         "tool.rkt")

(provide go/emit)

;;

(define +empty+     "")
(define +new-line+  "\n")
(define +tab+       "\t")
(define +comma+     ",")
(define +eq+        "=")
(define +space+     " ")
(define +lbracket+  "(")
(define +rbracket+  ")")
(define +lsbracket+ "[")
(define +rsbracket+ "]")
(define +lcbracket+ "{")
(define +rcbracket+ "}")
(define +backtick+  "`")
(define +asterisk+  "*")
(define +ampersand+ "&")

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

(define (emit-type ast)
  (match ast
    ((? symbol? ast)
     (*->string ast))
    ((go:type t)
     (emit-type t))
    ((go:type:id name #f)
     (*->string name))
    ((go:type:id (or 'slice 'array 'ptr 'chan) type)
     (emit-type type))
    ((go:type:id name type)
     (string-append (*->string name) (emit-type type)))

    ((go:type:id:map key value)
     (string-append +lsbracket+
                    (emit-type key)
                    +rsbracket+
                    (emit-type value)))

    ((or (go:type:id:struct fields)
         (go:type:id:interface fields))
     (string-append
      +lcbracket+
      +new-line+
      (string-join
       (for/list ((field fields))
         (string-append +tab+ (emit-type field)))
       +new-line+)
      +new-line+
      +rcbracket+))
    ((go:type:id:struct:field name type tag)
     (string-append
      (if name
          (string-append (*->string name) +space+)
          +empty+)
      (emit-type type)
      (if tag
          (string-append +space+ +backtick+ (*->string tag) +backtick+)
          +empty+)))
    ((go:type:id:interface:field name type)
     (string-append
      (if name
          (string-append (*->string name) +space+)
          +empty+)
      (emit-type type)))

    ((go:type:id:slice type)
     (string-append (emit-type type) +lsbracket+ +rsbracket+))
    ((go:type:id:array type size)
     (string-append (emit-type type) +lsbracket+ (*->string size) +rsbracket+))
    ((go:type:id:ptr   type)
     (string-append +asterisk+ (emit-type type)))
    ((go:type:id:chan  direction type)
     (string-append (*->string direction) "chan" +space+ (emit-type type)))

    ((go:type:id:func input output)
     (let ((io (lambda (v) (if (pair? v)
                               (string-append (emit-type (car v))
                                              +space+
                                              (emit-type (cdr v)))
                               (emit-type v)))))
       (string-append
        +lbracket+
        (string-join (map io input) +comma+)
        +rbracket+
        +lbracket+
        (string-join (map io output) +comma+)
        +rbracket+)))
    ))

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
               (test-suite "var")
               (test-suite "type"
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'int #f)))
                            "int")
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'int64 #f)))
                            "int64")
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'string #f)))
                            "string")
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'userDefined #f)))
                            "userDefined")

                           (check-equal?
                            (emit-type
                             (go:type (go:type:id (string->symbol "map[string]int") #f)))
                            "map[string]int")
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'map (go:type:id:map 'key 'value))))
                            "map[key]value")

                           (check-equal?
                            (emit-type
                             (go:type (go:type:id
                                       'struct
                                       (go:type:id:struct
                                        (list (go:type:id:struct:field #f (go:type:id 'io.Reader #f) #f)
                                              (go:type:id:struct:field 'x (go:type:id 'map (go:type:id:map (go:type:id 'string #f) (go:type:id 'string #f))) #f)
                                              (go:type:id:struct:field 'y (go:type:id 'X #f) #f))))))
                            (string-append "struct"
                                           +lcbracket+
                                           +new-line+ +tab+ "io.Reader"
                                           +new-line+ +tab+ "x map[string]string"
                                           +new-line+ +tab+ "y X"
                                           +new-line+
                                           +rcbracket+))
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id
                                       'struct
                                       (go:type:id:struct
                                        (list (go:type:id:struct:field #f (go:type:id 'io.Reader #f) "foo:bar")
                                              (go:type:id:struct:field 'x (go:type:id 'map (go:type:id:map (go:type:id 'string #f) (go:type:id 'string #f))) "bar:baz")
                                              (go:type:id:struct:field 'y (go:type:id 'X #f) "baz:qux"))))))
                            (string-append "struct"
                                           +lcbracket+
                                           +new-line+ +tab+ "io.Reader `foo:bar`"
                                           +new-line+ +tab+ "x map[string]string `bar:baz`"
                                           +new-line+ +tab+ "y X `baz:qux`"
                                           +new-line+
                                           +rcbracket+))
                           (check-equal?
                            (emit-type (go:type (go:type:id 'struct (go:type:id:struct (list)))))
                            (string-append "struct" +lcbracket+ +new-line+ +new-line+ +rcbracket+))
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id
                                       'interface
                                       (go:type:id:interface
                                        (list (go:type:id:interface:field #f (go:type:id 'io.Reader #f)))))))
                            (string-append "interface"
                                           +lcbracket+
                                           +new-line+ +tab+ "io.Reader"
                                           +new-line+
                                           +rcbracket+))

                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'slice (go:type:id:slice (go:type:id 'string #f)))))
                            "string[]")
                           (check-equal?
                            (emit-type (go:type (go:type:id 'slice
                                                            (go:type:id:slice
                                                             (go:type:id 'struct (go:type:id:struct (list)))))))
                            (string-append "struct" +lcbracket+
                                           +new-line+ +new-line+
                                           +rcbracket+ +lsbracket+ +rsbracket+))
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'array (go:type:id:array (go:type:id 'string #f)
                                                                           5))))
                            "string[5]")
                           (check-equal?
                            (emit-type (go:type (go:type:id
                                                 'array
                                                 (go:type:id:array (go:type:id 'struct (go:type:id:struct (list)))
                                                                   5))))
                            (string-append "struct" +lcbracket+
                                           +new-line+ +new-line+
                                           +rcbracket+ +lsbracket+ "5" +rsbracket+))
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'ptr (go:type:id:ptr (go:type:id 'string #f)))))
                            "*string")
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'chan (go:type:id:chan '-> (go:type:id 'string #f)))))
                            "->chan string")
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'chan (go:type:id:chan '<- (go:type:id 'string #f)))))
                            "<-chan string")
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'chan (go:type:id:chan #f (go:type:id 'string #f)))))
                            "chan string")

                           (check-equal?
                            (emit-type
                             (go:type
                              (go:type:id
                               'func
                               (go:type:id:func
                                (list (cons 'k (go:type:id 'string #f))
                                      (cons 'v (go:type:id 'int    #f)))
                                (list (go:type:id 'int   #f)
                                      (go:type:id 'error #f))))))
                            "func(k string,v int)(int,error)")
                           (check-equal?
                            (emit-type
                             (go:type
                              (go:type:id
                               'func
                               (go:type:id:func
                                (list)
                                (list (cons 'x   (go:type:id 'int   #f))
                                      (cons 'err (go:type:id 'error #f)))))))
                            "func()(x int,err error)")
                           (check-equal?
                            (emit-type
                             (go:type
                              (go:type:id
                               'func
                               (go:type:id:func
                                (list (go:type:id 'string #f)
                                      (go:type:id 'int    #f))
                                (list (go:type:id 'int   #f)
                                      (go:type:id 'error #f))))))
                            "func(string,int)(int,error)")))))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
