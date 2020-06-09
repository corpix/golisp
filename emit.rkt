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
(define +scomma+    ", ")
(define +lbracket+  "(")
(define +rbracket+  ")")
(define +lsbracket+ "[")
(define +rsbracket+ "]")
(define +lcbracket+ "{")
(define +rcbracket+ "}")
(define +backtick+  "`")
(define +asterisk+  "*")
(define +ampersand+ "&")
(define +colon+     ":")
(define +coloneq+   ":=")

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

(define (emit-var ast)
  (match ast
    ((go:var bindings)
     (string-append "var" +space+ +lbracket+ +new-line+
                    (string-join (map (lambda (v)
                                        (string-append +tab+ (emit-var v)))
                                      bindings)
                                 +new-line+)
                    +new-line+ +rbracket+))
    ((go:var:binding name type value)
     (string-append
      (symbol->string name) +space+ (emit-type type)
      (if value (string-append +space+ +eq+ +space+ (emit-expr value))
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
     (string-append +lcbracket+ +new-line+
		    (string-join
		     (for/list ((field fields))
		       (string-append +tab+ (emit-type field)))
		     +new-line+)
		    +new-line+
		    +rcbracket+))
    ((go:type:id:struct:field name type tag)
     (string-append (if name
			(string-append (*->string name) +space+)
			+empty+)
		    (emit-type type)
		    (if tag
			(string-append +space+ +backtick+ (*->string tag) +backtick+)
			+empty+)))
    ((go:type:id:interface:field name type)
     (string-append (if name
			(string-append (*->string name) +space+)
			+empty+)
		    (emit-type type)))

    ((go:type:id:slice type)
     (string-append +lsbracket+ +rsbracket+ (emit-type type)))
    ((go:type:id:array type size)
     (string-append +lsbracket+ (*->string size) +rsbracket+ (emit-type type)))
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
       (string-append +space+ +lbracket+
		      (string-join (map io input) +scomma+)
		      +rbracket+
		      +space+ +lbracket+
		      (string-join (map io output) +scomma+)
		      +rbracket+)))))

(define (emit-create ast)
  (match ast
    ((go:create (go:type:id kind type) expr)
     (string-append (emit-type (go:create-type ast))
                    (emit-create (list kind expr))))

    ((list (or (quote map)
               (quote struct))
           (list ast ...))
     (string-append
      +lcbracket+
      (string-join
       (map (lambda (kv)
              (if (pair? kv)
                  (string-append (emit-expr (car kv)) +colon+ +space+
                                 (emit-expr (cdr kv)))
                  (emit-expr kv))) ast) +scomma+) +rcbracket+))

    ((list (or (quote slice)
               (quote array))
           (list ast ...))
     (string-append
      +lcbracket+
      (string-join (map emit-expr ast) +scomma+)
      +rcbracket+))

    ((list _ (list ast ...))
     (string-append
      +lcbracket+
      (string-join (map emit-expr ast) +scomma+)
      +rcbracket+))
    ((list _ ast)
     (string-append +lbracket+ (emit-expr ast) +rbracket+))))

(define (emit-def ast)
  (match ast
    ((go:def id expr)
     (string-append (*->string id)
                    +space+ +coloneq+ +space+
                    (emit-expr expr)))))

(define (emit-set ast)
  (match ast
    ((go:set id expr)
     (string-append (*->string id)
                    +space+ +eq+ +space+
                    (emit-expr expr)))))

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

(define (emit-func ast)
  (match ast
    ((go:func name input output body)
     (string-append "func" +space+
                    (if name
                        (string-append (*->string name) +space+)
                        +empty+)
                    +lbracket+ (string-join (map emit-func input) +scomma+) +rbracket+
                    +space+
                    +lbracket+ (string-join (map emit-func output) +scomma+) +rbracket+
                    +space+
                    +lcbracket+
                    (if (pair? body)
                        (string-append +new-line+ (emit-expr body) +new-line+)
                        +empty+)
                    +rcbracket+))
    ((go:type:id v _)
     (*->string v))
    ((cons k v)
     (string-append (*->string k) +space+ (emit-func v)))))

(define (emit-id ast)     (~a ast))
(define (emit-string ast) (~s ast))
(define (emit-number ast) (~a ast))

(define (emit-expr ast)
  (match ast
    ((go:expr expr)       (emit-expr expr))
    ((? go:operator? ast) (emit-operator ast))
    ((? go:var?      ast) (emit-var      ast))
    ((? go:type?     ast) (emit-type     ast))
    ((? go:create?   ast) (emit-create   ast))
    ((? go:def?      ast) (emit-def      ast))
    ((? go:set?      ast) (emit-set      ast))
    ((? go:package?  ast) (emit-package  ast))
    ((? go:imports?  ast) (emit-imports  ast))
    ((? go:func?     ast) (emit-func     ast))


    ((? (lambda (v) (and (list? v)
                         (not (empty? v))
                         (symbol? (car v))))
        ast)
     (format "~a(~a)"
             (car ast)
             (string-join (map emit-expr (cdr ast))
                          +comma+)))

    ((go:expr exprs) (emit-expr exprs))

    ((quote nil)     "nil")
    ((? symbol? ast) (emit-id (symbol->string ast)))
    ((? string? ast) (emit-string ast))
    ((? number? ast) (emit-number ast))
    ((? list? ast)   (string-join
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

               (test-suite "var"
                           (check-equal?
                            (emit-var (go:var (list (go:var:binding 'x (go:type:id 'y #f) #f))))
                            (string-append "var" +space+ +lbracket+
                                           +new-line+ +tab+ "x y"
                                           +new-line+ +rbracket+))
                           (check-equal?
                            (emit-var (go:var (list (go:var:binding 'x (go:type:id 'y #f) (go:expr 1)))))
                            (string-append "var" +space+ +lbracket+
                                           +new-line+ +tab+ "x y = 1"
                                           +new-line+ +rbracket+))
                           (check-equal?
                            (emit-var (go:var (list (go:var:binding 'x  (go:type:id 'y #f)  (go:expr 1))
                                                    (go:var:binding 'xx (go:type:id 'yy #f) (go:expr 2)))))
                            (string-append "var" +space+ +lbracket+
                                           +new-line+ +tab+ "x y = 1"
                                           +new-line+ +tab+ "xx yy = 2"
                                           +new-line+ +rbracket+)))
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
                            "[]string")
                           (check-equal?
                            (emit-type (go:type (go:type:id 'slice
                                                            (go:type:id:slice
                                                             (go:type:id 'struct (go:type:id:struct (list)))))))
                            (string-append "[]struct" +lcbracket+
                                           +new-line+ +new-line+
                                           +rcbracket+))
                           (check-equal?
                            (emit-type
                             (go:type (go:type:id 'array (go:type:id:array (go:type:id 'string #f)
                                                                           5))))
                            "[5]string")
                           (check-equal?
                            (emit-type (go:type (go:type:id
                                                 'array
                                                 (go:type:id:array (go:type:id 'struct (go:type:id:struct (list)))
                                                                   5))))
                            (string-append "[5]struct" +lcbracket+
                                           +new-line+ +new-line+
                                           +rcbracket+))
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
                            "func (k string, v int) (int, error)")
                           (check-equal?
                            (emit-type
                             (go:type
                              (go:type:id
                               'func
                               (go:type:id:func
                                (list)
                                (list (cons 'x   (go:type:id 'int   #f))
                                      (cons 'err (go:type:id 'error #f)))))))
                            "func () (x int, err error)")
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
                            "func (string, int) (int, error)"))

	       (test-suite "create"
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'int #f) (go:expr 0)))
                            "int(0)")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'int64 #f) (go:expr 666)))
                            "int64(666)")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'X #f) (go:expr 'nil)))
                            "X(nil)")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'X #f) (list (go:expr 'nil))))
                            "X{nil}")
			   (check-equal?
                            (emit-create
                             (go:create (go:type:id 'slice (go:type:id:slice (go:type:id 'X #f)))
					(list (go:expr 1) (go:expr 2)
					      (go:expr 3) (go:expr 4))))
                            "[]X{1, 2, 3, 4}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'array (go:type:id:array (go:type:id 'X #f) 4))
                                        (list (go:expr 1) (go:expr 2)
                                              (go:expr 3) (go:expr 4))))
                            "[4]X{1, 2, 3, 4}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id
                                         'map
                                         (go:type:id:map (go:type:id 'string #f)
                                                         (go:type:id 'int    #f)))
                                        (list (cons "1" (go:expr 1))
                                              (cons "2" (go:expr 2))
                                              (cons "3" (go:expr 3))
                                              (cons "4" (go:expr 4)))))
                            "map[string]int{\"1\": 1, \"2\": 2, \"3\": 3, \"4\": 4}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id
                                         'map
                                         (go:type:id:map (go:type:id 'int    #f)
                                                         (go:type:id 'string #f)))
                                        (list (cons 1 (go:expr "1"))
                                              (cons 2 (go:expr "2"))
                                              (cons 3 (go:expr "3"))
                                              (cons 4 (go:expr "4")))))
                            "map[int]string{1: \"1\", 2: \"2\", 3: \"3\", 4: \"4\"}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'struct
                                                    (go:type:id:struct
                                                     (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                        (list (go:expr 1))))
                            "struct{\n\tx int\n}{1}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id
                                         'struct
                                         (go:type:id:struct
                                          (list (go:type:id:struct:field
                                                 'x
                                                 (go:type:id 'interface (go:type:id:interface null))
                                                 #f))))
                                        null))
                            "struct{\n\tx interface{\n\n}\n}{}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'struct
                                                    (go:type:id:struct (list (go:type:id:struct:field
                                                                              'x
                                                                              (go:type:id 'interface (go:type:id:interface null))
                                                                              #f))))
                                        null))
                            "struct{\n\tx interface{\n\n}\n}{}")
                           (check-equal?
                            (emit-create
                             (go:create
                              (go:type:id 'map
                                          (go:type:id:map
                                           (go:type:id 'string #f)
                                           (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))))
                              (list (cons "1" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                        (list (cons 'x (go:expr 1))))))
                                    (cons "2" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                        (list (cons 'x (go:expr 2))))))
                                    (cons "3" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                        (list (cons 'x (go:expr 3))))))
                                    (cons "4" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                        (list (cons 'x (go:expr 4)))))))))
                            "map[string]struct{\n\tx int\n}{\"1\": struct{\n\tx int\n}{x: 1}, \"2\": struct{\n\tx int\n}{x: 2}, \"3\": struct{\n\tx int\n}{x: 3}, \"4\": struct{\n\tx int\n}{x: 4}}"))

               (test-suite "def"
                           (check-equal?
                            (emit-def (go:def 'x (go:expr 1)))
                            "x := 1")
                           (check-equal?
                            (emit-def (go:def 'x (go:expr (go:func #f null null null))))
                            "x := func () () {}")
                           (check-equal?
                            (emit-def (go:def 'x (go:expr (go:create
                                                           (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                           null))))
                            "x := []int{}"))

               (test-suite "set"
                           (check-equal?
                            (emit-set (go:set 'x (go:expr 1)))
                            "x = 1")
                           (check-equal?
                            (emit-set (go:set 'x (go:expr (go:func #f null null null))))
                            "x = func () () {}")
                           (check-equal?
                            (emit-set (go:set 'x (go:expr (go:create
                                                           (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                           null))))
                            "x = []int{}"))

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

               (test-suite "func"
                           (check-equal?
                            (emit-func (go:func #f null null null))
                            "func () () {}")
                           (check-equal?
                            (emit-func (go:func 'hello null null null))
                            "func hello () () {}")
                           (check-equal?
                            (emit-func (go:func #f (list (go:type:id 't #f)) null null))
                            "func (t) () {}")
                           (check-equal?
                            (emit-func (go:func #f
                                                `((name        . ,(go:type:id 'type       #f)))
                                                `((returnName  . ,(go:type:id 'returnType #f)))
                                                null))
                            "func (name type) (returnName returnType) {}")
                           (check-equal?
                            (emit-func (go:func #f
                                                `((name         . ,(go:type:id 'type        #f))
                                                  (name1        . ,(go:type:id 'type1       #f)))
                                                `((returnName   . ,(go:type:id 'returnType  #f))
                                                  (returnName1  . ,(go:type:id 'returnType1 #f)))
                                                null))
                            "func (name type, name1 type1) (returnName returnType, returnName1 returnType1) {}")
                           (check-equal?
                            (emit-func (go:func #f
                                                `((name       . ,(go:type:id 'type       #f)))
                                                `((returnName . ,(go:type:id 'returnType #f)))
                                                (list (go:expr
                                                       (go:func #f
                                                                `((name1        . ,(go:type:id 'type1       #f)))
                                                                `((returnName1  . ,(go:type:id 'returnType1 #f)))
                                                                null)))))
                            "func (name type) (returnName returnType) {\nfunc (name1 type1) (returnName1 returnType1) {}\n}")
                           (check-equal?
                            (emit-func (go:func #f
                                                `((name        . ,(go:type:id 'type       #f)))
                                                `((returnName  . ,(go:type:id 'returnType #f)))
                                                (list (go:expr
                                                       (go:func #f
                                                                `((name1        . ,(go:type:id 'type1       #f)))
                                                                `((returnName1  . ,(go:type:id 'returnType1 #f)))
                                                                null))
                                                      (go:expr
                                                       (go:func #f
                                                                `((name1       . ,(go:type:id 'type1      #f)))
                                                                `((returnName1 . ,(go:type:id 'returnType1 #f)))
                                                                null)))))
                            "func (name type) (returnName returnType) {\nfunc (name1 type1) (returnName1 returnType1) {}\nfunc (name1 type1) (returnName1 returnType1) {}\n}")))))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
