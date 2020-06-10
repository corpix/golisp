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
(define +dot+       ".")
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
(define +semicolon+ ";")
(define +coloneq+   ":=")

;;

(define (emit-operator ast)
  (match ast
    ((go:operator id operands)
     (string-append
      +lbracket+
      (if (pair? (cdr operands))
          (string-join
           (map emit-expr operands)
           (string-append +space+ (symbol->string id) +space+))
          (string-append (symbol->string id) (emit-expr (car operands))))
      +rbracket+))))

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
  (let ((emit-item (lambda (kv)
                     (if (pair? kv)
                         (string-append (emit-expr (car kv)) +colon+ +space+
                                        (emit-expr (cdr kv)))
                         (emit-expr kv)))))
    (match ast
      ((go:create (go:type:id kind type) expr)
       (string-append (emit-type (go:create-type ast))
                      (emit-create (list kind expr))))

      ((list (or (quote map)
                 (quote struct))
             (list ast ...))
       (string-append
        +lcbracket+
        (string-join (map emit-item ast) +scomma+) +rcbracket+))

      ((list (or (quote slice)
                 (quote array))
             (list ast ...))
       (string-append
        +lcbracket+
        (string-join (map emit-item ast) +scomma+)
        +rcbracket+))

      ((list _ (list ast ...))
       (string-append
        +lcbracket+
        (string-join (map emit-item ast) +scomma+)
        +rcbracket+))
      ((list _ ast)
       (string-append +lbracket+ (emit-item ast) +rbracket+)))))


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
     (string-append (*->string k) +space+ (emit-func v)))
    ((? symbol? ast)
     (*->string ast))))

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

(define (emit-go ast)
  (match ast
    ((go:go func) (string-append "go" +space+ (emit-expr func)))))

(define (emit-if ast)
  (match ast
    ((go:if condition then false)
     (string-append "if" +space+ (emit-expr condition)
                    +space+ +lcbracket+ +new-line+
                    +tab+ (emit-expr then) +new-line+
                    +rcbracket+
                    (if false
                        (string-append +space+ "else" +space+ +lcbracket+ +new-line+
                                       +tab+ (emit-expr false) +new-line+
                                       +rcbracket+)
                        +empty+)))))

(define (emit-for ast)
  (match ast
    ((go:for vars seq pred iter kind body)
     (string-append "for"
                    (if (pair? vars)
                        (string-append
                         +space+
                         (string-join (map symbol->string vars) +scomma+)
                         +space+ +coloneq+ +space+
                         (if (and (eq? kind 'range) (not (pair? pred)))
                             (string-append (symbol->string kind) +space+)
                             +empty+)
                         (string-join (map emit-expr seq) +scomma+))
                        +empty+)
                    (if pred
                        (string-append +semicolon+ +space+ (emit-expr pred))
                        +empty+)
                    (if iter
                        (string-append +semicolon+ +space+ (emit-expr iter))
                        +empty+)
                    +space+
                    +lcbracket+ +new-line+
                    +tab+ (emit-expr body)
                    +new-line+ +rcbracket+))))

(define (emit-switch ast)
  (match ast
    ((go:switch value cases)
     (string-append "switch" +space+ (emit-expr value) +space+
                    +lcbracket+
                    (string-join (map emit-switch cases) +new-line+)
                    +rcbracket+))
    ((go:case predicate body)
     (string-append
      (cond ((eq? predicate (quote default)) "default")
            (#t (string-append "case" +space+ (emit-expr predicate))))
      +colon+ +new-line+ +tab+
      (string-join (map emit-expr body) +new-line+)))))

(define (emit-select ast)
  (match ast
    ((go:select cases)
     (string-append "select" +space+
                    +lcbracket+
                    (string-join (map emit-select cases) +new-line+)
                    +rcbracket+))
    ((go:case predicate body)
     (string-append
      (cond ((eq? predicate (quote default)) "default")
            (#t (string-append "case" +space+ (emit-expr predicate))))
      +colon+ +new-line+ +tab+
      (string-join (map emit-expr body) +new-line+)))))

(define (emit-cast ast)
  (match ast
    ((go:cast value (go:cast:assert type))
     (string-append +lbracket+ (emit-expr value) +rbracket+ +dot+ +lbracket+ (emit-type type) +rbracket+))
    ((go:cast value type)
     (string-append (emit-type type) +lbracket+ (emit-expr value) +rbracket+))))

(define (emit-return ast)
  (match ast
    ((go:return values)
     (string-append
      "return"
      (if (pair? values) +space+ +empty+)
      (string-join (map emit-expr values) +scomma+)))))

(define (emit-break ast)
  (match ast
    ((go:break label)
     (string-append
      "break"
      (if label (string-append +space+ (*->string label))
          +empty+)))))

(define (emit-continue ast)
  (match ast
    ((go:continue label)
     (string-append
      "continue"
      (if label (string-append +space+ (*->string label))
          +empty+)))))

(define (emit-label ast)
  (match ast
    ((go:label name body)
     (string-append (*->string name) +colon+ +new-line+
                    (emit-expr body)))))

(define (emit-goto ast)
  (match ast
    ((go:goto label)
     (string-append "goto" +space+ (*->string label)))))

(define (emit-iota ast)
  (match ast
    ((go:iota) "iota")))

(define (emit-defer ast)
  (match ast
    ((go:defer body) (string-append "defer" +space+ (emit-expr body)))))

(define (emit-slice ast)
  (match ast
    ((go:slice value start end)
     (string-append (emit-expr value)
                    +lsbracket+
                    (emit-expr start)
                    +colon+
                    (if end (emit-expr end) +empty+)
                    +rsbracket+))))

(define (emit-index ast)
  (match ast
    ((go:index value key)
     (string-append (emit-expr value)
                    +lsbracket+ (emit-expr key) +rsbracket+))))

(define (emit-send ast)
  (match ast
    ((go:send chan value)
     (string-append (emit-expr chan) "<-" (emit-expr value)))))
(define (emit-receive ast)
  (match ast
    ((go:receive chan)
     (string-append "<-" (emit-expr chan)))))

(define (emit-inc ast)
  (match ast
    ((go:inc id) (string-append (*->string id) "++"))))

(define (emit-dec ast)
  (match ast
    ((go:dec id) (string-append (*->string id) "--"))))

(define (emit-ref ast)
  (match ast
    ((go:ref expr) (string-append "&" (emit-expr expr)))))

(define (emit-deref ast)
  (match ast
    ((go:deref expr) (string-append "*"(emit-expr expr)))))


(define (emit-id ast)     (~a ast))
(define (emit-string ast) (~s ast))
(define (emit-number ast) (~a ast))

(define (emit-expr ast)
  (match ast
    ((go:expr expr)       (emit-expr expr))
    ((? go:operator? ast) (emit-operator ast))
    ((? go:type?     ast) (emit-type     ast))
    ((? go:create?   ast) (emit-create   ast))
    ((? go:def?      ast) (emit-def      ast))
    ((? go:set?      ast) (emit-set      ast))
    ((? go:package?  ast) (emit-package  ast))
    ((? go:imports?  ast) (emit-imports  ast))
    ((? go:func?     ast) (emit-func     ast))
    ((? go:var?      ast) (emit-var      ast))
    ((? go:go?       ast) (emit-go       ast))
    ((? go:if?       ast) (emit-if       ast))
    ((? go:for?      ast) (emit-for      ast))
    ((? go:switch?   ast) (emit-switch   ast))
    ((? go:select?   ast) (emit-select   ast))
    ((? go:cast?     ast) (emit-cast     ast))
    ((? go:return?   ast) (emit-return   ast))
    ((? go:break?    ast) (emit-break    ast))
    ((? go:continue? ast) (emit-continue ast))
    ((? go:label?    ast) (emit-label    ast))
    ((? go:goto?     ast) (emit-goto     ast))
    ((? go:iota?     ast) (emit-iota     ast))
    ((? go:defer?    ast) (emit-defer    ast))
    ((? go:slice?    ast) (emit-slice    ast))
    ((? go:index?    ast) (emit-index    ast))
    ((? go:send?     ast) (emit-send     ast))
    ((? go:receive?  ast) (emit-receive  ast))

    ((? go:receive?  ast) (emit-receive  ast))
    ((? go:inc?      ast) (emit-inc      ast))
    ((? go:dec?      ast) (emit-dec      ast))
    ((? go:ref?      ast) (emit-ref      ast))
    ((? go:deref?    ast) (emit-deref    ast))

    ((go:func:call func arguments)
     (string-append (emit-func func)
                    +lbracket+
                    (string-join (map emit-expr arguments) +scomma+)
                    +rbracket+))

    ((? (lambda (v) (and (list? v)
                         (not (empty? v))
                         (symbol? (car v))))
        ast)
     (string-append (car ast)
                    +lbracket+
                    (string-join (map emit-expr (cdr ast))
                                 +comma+)
                    +rbracket+))

    ((go:expr exprs) (emit-expr exprs))

    ((quote nil) "nil")
    (#t          "true")
    (#f          "false")

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
                            (emit-func 'Foo)
                            "Foo")
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
                            "func (name type) (returnName returnType) {\nfunc (name1 type1) (returnName1 returnType1) {}\nfunc (name1 type1) (returnName1 returnType1) {}\n}"))

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

               (test-suite "go"
                           (check-equal?
                            (emit-go (go:go (go:func:call (go:func #f null null null) null)))
                            "go func () () {}()"))

               (test-suite "if"
                           (check-equal?
                            (emit-if (go:if
                                      (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                                      (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                      #f))
                            "if (1 == 1) {\n\tfmt.Println(\"ok\")\n}")
                           (check-equal?
                            (emit-if (go:if
                                      (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                                      (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                      (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))
                            "if (1 == 1) {\n\tfmt.Println(\"ok\")\n} else {\n\tfmt.Println(\"not ok\")\n}")
                           (check-equal?
                            (emit-if (go:if
                                      (go:expr (go:operator '!
                                                            (list (go:expr
                                                                   (go:operator '==
                                                                                (list (go:expr 1) (go:expr 1)))))))
                                      (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                      (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))
                            "if (!(1 == 1)) {\n\tfmt.Println(\"ok\")\n} else {\n\tfmt.Println(\"not ok\")\n}")
                           (check-equal?
                            (emit-if (go:if
                                      (go:expr (go:operator '!
                                                            (list (go:expr
                                                                   (go:operator
                                                                    '==
                                                                    (list (go:expr
                                                                           (go:operator '+ (list (go:expr 1) (go:expr 5))))
                                                                          (go:expr 1)))))))
                                      (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                      (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))
                            "if (!((1 + 5) == 1)) {\n\tfmt.Println(\"ok\")\n} else {\n\tfmt.Println(\"not ok\")\n}"))

               (test-suite "for"
                           (check-equal?
                            (emit-for (go:for
                                       null null #f #f #f
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                            "for {\n\tfmt.Println(k, v)\n}")
                           (check-equal?
                            (emit-for (go:for
                                       (list 'k 'v)
                                       (list (go:expr
                                              (go:create
                                               (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                               (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                                       #f #f #f
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                            "for k, v := []int{1, 2, 3} {\n\tfmt.Println(k, v)\n}")
                           (check-equal?
                            (emit-for (go:for
                                       (list 'k 'v)
                                       (list (go:expr
                                              (go:create
                                               (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                               (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                                       #f #f 'range
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                            "for k, v := range []int{1, 2, 3} {\n\tfmt.Println(k, v)\n}")
                           (check-equal?
                            (emit-for (go:for
                                       (list 'k)
                                       (list (go:expr 10))
                                       (go:expr (go:operator '> (list (go:expr 'k) (go:expr 0))))
                                       (go:expr (go:dec 'k))
                                       #f
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k)))))))
                            "for k := 10; (k > 0); k-- {\n\tfmt.Println(k)\n}"))

               (test-suite "switch"
                           (check-equal?
                            (emit-switch
                             (go:switch (go:expr 1)
                                        (list (go:case
                                               (go:expr 1)
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                              (go:case
                                               (go:expr 2)
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                              (go:case
                                               'default
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default")))))))))
                            "switch 1 {case 1:\n\tfmt.Println(\"one\")\ncase 2:\n\tfmt.Println(\"two\")\ndefault:\n\tfmt.Println(\"default\")}")
                           (check-equal?
                            (emit-switch
                             (go:switch (go:expr (go:operator '+ (list (go:expr 1) (go:expr 1))))
                                        (list (go:case
                                               (go:expr 1)
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                              (go:case
                                               (go:expr 2)
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                              (go:case
                                               'default
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default")))))))))
                            "switch (1 + 1) {case 1:\n\tfmt.Println(\"one\")\ncase 2:\n\tfmt.Println(\"two\")\ndefault:\n\tfmt.Println(\"default\")}"))

               (test-suite "select"
                           (check-equal?
                            (emit-select
                             (go:select
                              (list
                               (go:case
                                'default
                                (list (go:expr (go:func:call 'println (list (go:expr "default case")))))))))
                            "select {default:\n\tprintln(\"default case\")}")
                           (check-equal?
                            (emit-select
                             (go:select
                              (list (go:case
                                     (go:expr (go:def 'x (go:expr (go:receive (go:expr 'ch)))))
                                     (list (go:expr
                                            (go:func:call
                                             'fmt.Printf
                                             (list (go:expr "x: %+v\n") (go:expr 'x))))))
                                    (go:case
                                     'default
                                     (list (go:expr
                                            (go:func:call
                                             'fmt.Println
                                             (list (go:expr "default case")))))))))
                            "select {case x := <-ch:\n\tfmt.Printf(\"x: %+v\\n\", x)\ndefault:\n\tfmt.Println(\"default case\")}"))

               (test-suite "cast"
                           (check-equal?
                            (emit-cast (go:cast (go:expr 'v) (go:type:id 'bool #f)))
                            "bool(v)")
                           (check-equal?
                            (emit-cast (go:cast (go:expr 'v) (go:cast:assert (go:type:id 'bool #f))))
                            "(v).(bool)"))

               (test-suite "return"
                           (check-equal? (emit-return (go:return null)) "return")
                           (check-equal? (emit-return (go:return
                                                       (list (go:expr
                                                              (go:operator '+ (list
                                                                               (go:expr 1)
                                                                               (go:expr 1)))))))
                                         "return (1 + 1)")
                           (check-equal? (emit-return (go:return (list (go:expr 1) (go:expr 1))))
                                         "return 1, 1"))
               (test-suite "break"
                           (check-equal? (emit-break (go:break #f))   "break")
                           (check-equal? (emit-break (go:break 'xxx)) "break xxx"))

               (test-suite "continue"
                           (check-equal? (emit-continue (go:continue #f))   "continue")
                           (check-equal? (emit-continue (go:continue 'xxx)) "continue xxx"))

               (test-suite "label"
                           (check-equal? (emit-label
                                          (go:label 'xxx
                                                    (go:expr (go:for null null #f #f #f
                                                                     (list (go:expr (go:break 'xxx)))))))
                                         "xxx:\nfor {\n\tbreak xxx\n}"))

               (test-suite "goto"
                           (check-equal? (emit-goto (go:goto 'xxx)) "goto xxx"))

               (test-suite "iota"
                           (check-equal? (emit-iota (go:iota)) "iota"))

               (test-suite "defer"
                           (check-equal? (emit-defer (go:defer (go:expr (go:func:call 'println (list (go:expr "hello"))))))
                                         "defer println(\"hello\")"))

               (test-suite "slice"
                           (check-equal?
                            (emit-slice
                             (go:slice (go:expr 'arr)
                                       (go:expr 5)
                                       (go:expr 10)))
                            "arr[5:10]")
                           (check-equal?
                            (emit-slice
                             (go:slice (go:expr 'arr)
                                       (go:expr 5)
                                       #f))
                            "arr[5:]")
                           (check-equal?
                            (emit-slice
                             (go:slice (go:expr 'arr)
                                       (go:expr 0)
                                       (go:expr 10)))
                            "arr[0:10]")
                           (check-equal?
                            (emit-slice
                             (go:slice (go:expr 'arr)
                                       (go:expr (go:operator '+ (list (go:expr 1) (go:expr 1))))
                                       (go:expr 10)))
                            "arr[(1 + 1):10]"))

               (test-suite "index"
                           (check-equal?
                            (emit-index (go:index (go:expr 'arr) (go:expr 0)))
                            "arr[0]")
                           (check-equal?
                            (emit-index (go:index (go:expr 'arr)
                                                  (go:expr (go:operator '*
                                                                        (list (go:expr 2) (go:expr 'x))))))
                            "arr[(2 * x)]"))

               (test-suite "send"
                           (check-equal?
                            (emit-send (go:send (go:expr 'ch)
                                                (go:expr "test")))
                            "ch<-\"test\""))

               (test-suite "receive"
                           (check-equal?
                            (emit-receive (go:receive (go:expr 'ch)))
                            "<-ch"))

               (test-suite "inc"
                           (check-equal? (emit-inc (go:inc 'n)) "n++"))

               (test-suite "dec"
                           (check-equal? (emit-dec (go:dec 'n)) "n--"))

               (test-suite "ref"
                           (check-equal? (emit-ref (go:ref (go:expr 'v))) "&v"))

               (test-suite "deref"
                           (check-equal? (emit-deref (go:deref (go:expr 'v))) "*v"))

               (test-suite "primitive"
                           (test-case "nil"
                             (check-equal? (go/emit (go:expr 'nil))
                                           "nil"))
                           (test-case "bool"
                             (check-equal? (go/emit (go:expr #t)) "true")
                             (check-equal? (go/emit (go:expr #f)) "false"))
                           (test-case "number"
                             (check-equal? (go/emit (go:expr 666))    "666")
                             (check-equal? (go/emit (go:expr 666.6))  "666.6")
                             (check-equal? (go/emit (go:expr -666))   "-666")
                             (check-equal? (go/emit (go:expr -666.6)) "-666.6"))
                           (test-case "string"
                             (check-equal? (go/emit (go:expr "hello")) "\"hello\""))
                           (test-case "identifier"
                             (check-equal? (go/emit (go:expr 'runtime.GOMAXPROCS))
                                           "runtime.GOMAXPROCS")))
               (test-suite "dummy"
                           (check-equal?
                            (emit-expr
                             (list (go:expr (go:package 'main))
                                   (go:expr (go:imports (list (go:import 'os #f) (go:import 'fmt #f))))
                                   (go:expr (go:func 'main null null (list (go:expr (go:func:call 'println (list (go:expr 'os.Args)))))))))
                            "package main\nimport (\n\tos\n\tfmt\n)\nfunc main () () {\nprintln(os.Args)\n}"))



               (test-suite "complex"
                           (test-case "cli"
                             (check-equal?
                              (go/emit (list (go:expr (go:package 'main))
                                             (go:expr (go:imports (list (go:import 'os #f)
                                                                        (go:import 'fmt #f)
                                                                        (go:import 'github.com/urfave/cli/v2 'cli))))
                                             (go:expr (go:var (list (go:var:binding
                                                                     'Flags
                                                                     (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag #f)))
                                                                     (go:expr (go:create
                                                                               (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag #f)))
                                                                               (list (go:expr (go:create
                                                                                               (go:type:id 'cli.BoolFlag #f)
                                                                                               (list (cons 'Name  (go:expr "test"))
                                                                                                     (cons 'Usage (go:expr "test flag"))))))))))))
                                             (go:expr (go:func 'RootAction
                                                               (list (cons 'ctx (go:type:id 'ptr (go:type:id:ptr (go:type:id 'cli.Context #f)))))
                                                               (list (go:type:id 'error #f))
                                                               (list (go:expr
                                                                      (go:func:call 'fmt.Println
                                                                                    (list (go:expr "hello from root, test is")
                                                                                          (go:expr (go:func:call 'ctx.Bool
                                                                                                                 (list (go:expr "test"))))))))))
                                             (go:expr (go:func 'main
                                                               null null
                                                               (list (go:expr (go:def
                                                                               'app
                                                                               (go:expr (go:create
                                                                                         (go:type:id 'ptr
                                                                                                     (go:type:id:ptr (go:type:id 'cli.App #f)))
                                                                                         null))))
                                                                     (go:expr (go:set 'app.Flags  (go:expr 'Flags)))
                                                                     (go:expr (go:set 'app.Action (go:expr 'RootAction)))
                                                                     (go:expr (go:func:call 'app.Run
                                                                                            (list (go:expr 'os.Args)))))))))
                              "package main\nimport (\n\tos\n\tfmt\n\tcli github.com/urfave/cli/v2\n)\nvar (\n\tFlags []cli.Flag = []cli.Flag{cli.BoolFlag{Name: \"test\", Usage: \"test flag\"}}\n)\nfunc RootAction (ctx ptr) (error) {\nfmt.Println(\"hello from root, test is\", ctx.Bool(\"test\"))\n}\nfunc main () () {\napp := *cli.App{}\napp.Flags = Flags\napp.Action = RootAction\napp.Run(os.Args)\n}")))

               )))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
