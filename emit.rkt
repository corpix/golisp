#lang racket/base
(require racket/match
         racket/string
         racket/format
         racket/list
         "type.rkt"
         (except-in "tool.rkt" flatten))

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
(define +quote+     "'")
(define +dquote+    "\"")
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
    ((go:operator id (list operands ...))
     (string-append
      +lbracket+
      (if (pair? (cdr operands))
          (string-join
           (map emit-expr operands)
           (string-append +space+ (symbol->string id) +space+))
          (string-append (symbol->string id) (emit-expr (car operands))))
      +rbracket+))
    ((go:operator id operand)
     (emit-operator (go:operator id (list operand))))))

(define (emit-type ast)
  (match ast
    ((? symbol? ast)
     (*->string ast))

    ((go:type name type alias?)
     (if name
         (string-append "type" +space+ (*->string name)
                        (if alias? (string-append +space+ +eq+ +space+)
                            +space+)
                        (emit-type type))
         (emit-type type)))
    ((? (lambda (v) (and (go:type:id? v)
                         (eq? (go:type:id-name v)
                              (go:type:id-type v))))
        ast)
     (*->string (go:type:id-name ast)))

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
		    +new-line+ +rcbracket+))
    ((go:type:id:struct:field name type tag)
     (string-append (if name
			(string-append (*->string name) +space+)
			+empty+)
		    (emit-type type)
		    (if tag
			(string-append +space+ +backtick+ (*->string tag) +backtick+)
			+empty+)))
    ((go:type:id:interface:field name type)
     (string-append (string-append
                     (*->string name)
                     (if type
                         (let ((func (go:type:id-type type)))
                           (if func (emit-type func) (emit-type type)))
                         +empty+))))

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
      ((go:create (go:ref (go:type:id kind type)) expr)
       (string-append (emit-ref (go:create-type ast))
                      (emit-create (list kind expr))))
      ((go:create (go:deref (go:type:id kind type)) expr)
       (string-append (emit-deref (go:create-type ast))
                      (emit-create (list kind expr))))
      ((go:create (go:type:id kind type) expr)
       (string-append (emit-type (go:create-type ast))
                      (emit-create (list kind expr))))

      ((list (or (quote map)
                 (quote struct))
             (list ast ...))
       (string-append
        +lcbracket+
        (if (not (null? ast))
            (string-append +new-line+
                           (string-join (map emit-item ast)
                                        (string-append +comma+ +new-line+))
                           +comma+ +new-line+)
            +empty+)
        +rcbracket+))

      ((list (or (quote slice)
                 (quote array))
             (list ast ...))
       (string-append
        +lcbracket+
        (if (not (null? ast))
            (string-append +new-line+
                           (string-join (map emit-item ast)
                                        (string-append +comma+ +new-line+))
                           +comma+ +new-line+)
            +empty+)
        +rcbracket+))

      ((list _ (list ast ...))
       (string-append
        +lcbracket+
        (if (not (null? ast))
            (string-append +new-line+
                           (string-join (map emit-item ast)
                                        (string-append +comma+ +new-line+))
                           +comma+ +new-line+)
            +empty+)
        +rcbracket+))
      ((list _ ast)
       (string-append +lbracket+ (emit-item ast) +rbracket+)))))


(define (emit-def ast)
  (match ast
    ((go:def (list id ...) (list expr ...))
     (string-append (string-join (map emit-expr id) +scomma+)
                    +space+ +coloneq+ +space+
                    (string-join (map emit-expr expr)
                                 +scomma+)))
    ((go:set (list id ...) (list expr ...))
     (string-append (string-join (map emit-expr id) +scomma+)
                    +space+ +eq+ +space+
                    (string-join (map emit-expr expr)
                                 +scomma+)))))

(define emit-set emit-def)

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
      (string-append +dquote+ (*->string package) +dquote+)))))

(define (emit-func ast)
  (match ast
    ((go:func (cons struct-type struct-binding) name input output body)
     (string-append "func" +space+
                    (if struct-type
                        (string-append +lbracket+
                                       (if struct-binding
                                           (string-append (emit-id struct-binding) +space+)
                                           +empty+)
                                       (emit-type struct-type)
                                       +rbracket+
                                       +space+)
                        +empty+)
                    (if name (string-append (*->string name) +space+) +empty+)
                    (emit-func input) +space+ (emit-func output)
                    +space+
                    +lcbracket+
                    (if (pair? body)
                        (string-append +new-line+
                                       (string-join
                                        (map (lambda (expr) (string-append +tab+ (emit-expr expr))) body)
                                        +new-line+)
                                       +new-line+)
                        +empty+)
                    +rcbracket+))
    ((list xs ...)
     (string-append +lbracket+
                    (string-join (map emit-func ast) +scomma+)
                    +rbracket+))
    ((? go:type:id? ast)
     (emit-type ast))
    ((go:func:type:variadic type)
     (string-append "..." (emit-type type)))
    ((cons sym (go:func:type:variadic type))
     (string-append (*->string sym) +space+ "..." (emit-type type)))
    ((cons sym type)
     (string-append (*->string sym) +space+ (emit-type type)))
    ((? symbol? ast)
     (*->string ast))))

(define (emit-var ast)
  (let ((emit-bindings (lambda (kind bindings)
                         (string-append kind +space+ +lbracket+ +new-line+
                                        (string-join (map (lambda (v)
                                                            (string-append +tab+ (emit-var v)))
                                                          bindings)
                                                     +new-line+)
                                        +new-line+ +rbracket+))))
    (match ast
      ((go:var bindings)   (emit-bindings "var"   bindings))
      ((go:const bindings) (emit-bindings "const" bindings))

      ((go:var:binding name type value)
       (string-append
        (symbol->string name)
        (if type (string-append +space+ (emit-type type))
            +empty+)
        (if value (string-append +space+ +eq+ +space+ (emit-expr value))
            +empty+))))))

(define emit-const emit-var)

(define (emit-go ast)
  (match ast
    ((go:go func) (string-append "go" +space+ (emit-expr func)))))

(define (emit-if ast)
  (define (transform ast)
    (match ast
      ((go:expr expr)   (transform expr))
      ((go:begin exprs)  exprs)
      (_ (list ast))))
  (match ast
    ((go:if condition then else)
     (string-append "if" +space+ (emit-expr condition)
                    +space+ +lcbracket+ +new-line+
                    +tab+ (string-join (map emit-expr (transform then))
                                       +new-line+)
                    +new-line+
                    +rcbracket+
                    (if else
                        (string-append +space+ "else" +space+ +lcbracket+ +new-line+
                                       +tab+ (string-join (map emit-expr (transform else))
                                                          +new-line+)
                                       +new-line+
                                       +rcbracket+)
                        +empty+)))))

(define (emit-alias ast)
  (match-let (((go:alias namespace syms) ast))
    (let ((buckets (make-hasheq)))
      (for ((v (in-list syms)))
        (cond
          ((go:alias:const? v) (hash-set-cons! buckets 'const (go:alias:const-sym v)))
          ((go:alias:type?  v) (hash-set-cons! buckets 'type  (go:alias:type-sym  v)))
          ((or (and (not (list? v)) (cons? v))
               (string? v))
           (hash-set-cons! buckets 'var   v))
          (#t (error (format "symbol ~a has not matched any folding function" v)))))
      (string-join
       (for/fold ((acc null))
                 ((k (in-list (list 'const 'type 'var))))
         (let ((bucket (hash-ref buckets k #f)))
           (if bucket
               (cons (string-append
                      (symbol->string k) +space+ +lbracket+ +new-line+
                      (string-join
                       (for/fold ((acc null))
                                 ((v (in-list bucket)))
                         (cons (cond
                                 ((and (not (list? v)) (cons? v))
                                  (string-append +tab+ (car v) +space+ +eq+ +space+ namespace +dot+ (cdr v)))
                                 ((string? v)
                                  (string-append +tab+ v +space+ +eq+ +space+ namespace +dot+ v))
                                 (#t (error (format "symbol ~a has not matched any folding function" v))))
                               acc))
                       +new-line+)
                      +new-line+ +rbracket+)
                     acc)
               acc)))
       +new-line+))))

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

(define (emit-begin ast)
  (match ast
    ((go:begin exprs)
     (string-append +lcbracket+ +new-line+
                    (string-join (map emit-expr exprs)
                                 +new-line+)
                    +new-line+ +rcbracket+))))

(define (emit-switch ast)
  (match ast
    ((go:switch value cases)
     (string-append "switch" +space+
                    (if (not (null? value))
                        (string-append (emit-expr value) +space+)
                        +empty+)
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

(define (emit-spread ast)
  (match ast
    ((go:spread expr)
     (string-append (emit-expr expr) "..."))))

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

(define (emit-key ast)
  (match ast
    ((go:key object key)
     (string-append (emit-expr object)
                    +dot+
                    (string-join (map symbol->string key)
                                 +dot+)))))

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
    ((go:ref (? go:type:id?)) (emit-ref (emit-type (go:ref-expr ast))))
    ((go:ref expr)            (emit-ref (emit-expr expr)))
    ((? string? ast)          (string-append "&" ast))))

(define (emit-deref ast)
  (match ast
    ((go:deref (? go:type:id?)) (emit-deref (emit-type (go:deref-expr ast))))
    ((go:deref expr)            (emit-deref (emit-expr expr)))
    ((? string? ast)            (string-append "*" ast))))


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
    ((? go:const?    ast) (emit-var      ast))
    ((? go:go?       ast) (emit-go       ast))
    ((? go:if?       ast) (emit-if       ast))
    ((? go:alias?    ast) (emit-alias    ast))
    ((? go:for?      ast) (emit-for      ast))
    ((? go:begin?    ast) (emit-begin    ast))
    ((? go:switch?   ast) (emit-switch   ast))
    ((? go:select?   ast) (emit-select   ast))
    ((? go:cast?     ast) (emit-cast     ast))
    ((? go:return?   ast) (emit-return   ast))
    ((? go:break?    ast) (emit-break    ast))
    ((? go:continue? ast) (emit-continue ast))
    ((? go:spread?   ast) (emit-spread   ast))
    ((? go:label?    ast) (emit-label    ast))
    ((? go:goto?     ast) (emit-goto     ast))
    ((? go:iota?     ast) (emit-iota     ast))
    ((? go:defer?    ast) (emit-defer    ast))
    ((? go:slice?    ast) (emit-slice    ast))
    ((? go:index?    ast) (emit-index    ast))
    ((? go:key?      ast) (emit-key      ast))
    ((? go:send?     ast) (emit-send     ast))
    ((? go:receive?  ast) (emit-receive  ast))

    ((? go:receive?  ast) (emit-receive  ast))
    ((? go:inc?      ast) (emit-inc      ast))
    ((? go:dec?      ast) (emit-dec      ast))
    ((? go:ref?      ast) (emit-ref      ast))
    ((? go:deref?    ast) (emit-deref    ast))

    ((go:func:call func arguments)
     (string-append (emit-expr func)
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
                           (for ((op (list "+" "-" "%" "*" "/" "==" "!=" ">" "<" ">=" "<="
                                           "!" "&&" "||"
                                           "&"  "|" "^" "<<" ">>")))
                             (let ((operator (string->symbol op)))
                               (check-equal? (emit-operator (go:operator operator
                                                                         (list
                                                                          (go:expr 1)
                                                                          (go:expr 2))))
                                             (string-append +lbracket+ "1" +space+ (*->string operator) +space+  "2" +rbracket+))
                               (check-equal? (emit-operator (go:operator operator (go:expr 1)))
                                             (string-append +lbracket+ (*->string operator) "1" +rbracket+))
                               (check-equal? (emit-operator (go:operator operator (list (go:expr 1))))
                                             (string-append +lbracket+ (*->string operator) "1" +rbracket+))
                               (check-equal? (emit-operator (go:operator operator
                                                                         (list
                                                                          (go:expr 1)
                                                                          (go:expr 2)
                                                                          (go:expr 3))))
                                             (string-append +lbracket+ "1" +space+
                                                            (*->string operator) +space+ "2" +space+
                                                            (*->string operator) +space+ "3" +rbracket+))
                               (check-equal? (emit-operator (go:operator operator
                                                                         (list
                                                                          (go:expr 1)
                                                                          (go:expr 2)
                                                                          (go:expr (go:operator operator
                                                                                                (list
                                                                                                 (go:expr 1)
                                                                                                 (go:expr 2)
                                                                                                 (go:expr 3)))))))
                                             (string-append +lbracket+ "1" +space+
                                                            (*->string operator) +space+ "2" +space+
                                                            (*->string operator) +space+
                                                            (string-append +lbracket+ "1" +space+
                                                                           (*->string operator) +space+ "2" +space+
                                                                           (*->string operator) +space+ "3" +rbracket+)
                                                            +rbracket+)))))

               (test-suite "type"
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'int 'int) #f))
                            "int")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'int64 'int64) #f))
                            "int64")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'string 'string) #f))
                            "string")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'userDefined 'userDefined) #f))
                            "userDefined")

                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id (string->symbol "map[string]int") (string->symbol "map[string]int")) #f))
                            "map[string]int")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'map (go:type:id:map 'key 'value)) #f))
                            "map[key]value")

                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id
                                          'struct
                                          (go:type:id:struct
                                           (list (go:type:id:struct:field #f (go:type:id 'io.Reader 'io.Reader) #f)
                                                 (go:type:id:struct:field 'x (go:type:id 'map (go:type:id:map (go:type:id 'string 'string)
                                                                                                              (go:type:id 'string 'string)))
                                                                          #f)
                                                 (go:type:id:struct:field 'y (go:type:id 'X 'X)
                                                                          #f))))
                                      #f))
                            (string-append "struct"
                                           +lcbracket+
                                           +new-line+ +tab+ "io.Reader"
                                           +new-line+ +tab+ "x map[string]string"
                                           +new-line+ +tab+ "y X"
                                           +new-line+
                                           +rcbracket+))
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id
                                          'struct
                                          (go:type:id:struct
                                           (list (go:type:id:struct:field #f (go:type:id 'io.Reader 'io.Reader) "foo:bar")
                                                 (go:type:id:struct:field 'x (go:type:id 'map (go:type:id:map (go:type:id 'string 'string)
                                                                                                              (go:type:id 'string 'string)))
                                                                          "bar:baz")
                                                 (go:type:id:struct:field 'y (go:type:id 'X 'X) "baz:qux"))))
                                      #f))
                            (string-append "struct"
                                           +lcbracket+
                                           +new-line+ +tab+ "io.Reader `foo:bar`"
                                           +new-line+ +tab+ "x map[string]string `bar:baz`"
                                           +new-line+ +tab+ "y X `baz:qux`"
                                           +new-line+
                                           +rcbracket+))
                           (check-equal?
                            (emit-type (go:type #f (go:type:id 'struct (go:type:id:struct (list))) #f))
                            (string-append "struct" +lcbracket+ +new-line+ +new-line+ +rcbracket+))
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id
                                          'interface
                                          (go:type:id:interface
                                           (list (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))
                                      #f))
                            "interface{\n\tx () ()\n}")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id
                                          'interface
                                          (go:type:id:interface
                                           (list (go:type:id:interface:field #f (go:type:id 'io.Reader 'io.Reader)))))
                                      #f))
                            "interface{\n\tio.Reader\n}")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id
                                          'interface
                                          (go:type:id:interface
                                           (list (go:type:id:interface:field #f (go:type:id 'io.Reader 'io.Reader))
                                                 (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))
                                      #f))
                            "interface{\n\tio.Reader\n\tx () ()\n}")

                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'slice (go:type:id:slice (go:type:id 'string 'string))) #f))
                            "[]string")
                           (check-equal?
                            (emit-type (go:type #f (go:type:id 'slice
                                                               (go:type:id:slice
                                                                (go:type:id 'struct (go:type:id:struct (list)))))
                                                #f))
                            (string-append "[]struct" +lcbracket+
                                           +new-line+ +new-line+
                                           +rcbracket+))
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'array
                                                     (go:type:id:array
                                                      (go:type:id 'string 'string)
                                                      5))
                                      #f))
                            "[5]string")
                           (check-equal?
                            (emit-type (go:type #f (go:type:id
                                                    'array
                                                    (go:type:id:array (go:type:id 'struct (go:type:id:struct (list)))
                                                                      5))
                                                #f))
                            (string-append "[5]struct" +lcbracket+
                                           +new-line+ +new-line+
                                           +rcbracket+))
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'ptr (go:type:id:ptr (go:type:id 'string 'string))) #f))
                            "*string")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'chan (go:type:id:chan '-> (go:type:id 'string 'string))) #f))
                            "->chan string")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'chan (go:type:id:chan '<- (go:type:id 'string 'string))) #f))
                            "<-chan string")
                           (check-equal?
                            (emit-type
                             (go:type #f (go:type:id 'chan (go:type:id:chan #f (go:type:id 'string 'string))) #f))
                            "chan string")

                           (check-equal?
                            (emit-type
                             (go:type 'name
                                      (go:type:id 'chan
                                                  (go:type:id:chan #f
                                                                   (go:type:id 'struct (go:type:id:struct null))))
                                      #f))
                            "type name chan struct{\n\n}")

                           (check-equal?
                            (emit-type
                             (go:type
                              #f
                              (go:type:id
                               'func
                               (go:type:id:func
                                (list (cons 'k (go:type:id 'string 'string))
                                      (cons 'v (go:type:id 'int 'int)))
                                (list (go:type:id 'int 'int)
                                      (go:type:id 'error 'error))))
                              #f))
                            "func (k string, v int) (int, error)")
                           (check-equal?
                            (emit-type
                             (go:type
                              #f
                              (go:type:id
                               'func
                               (go:type:id:func
                                (list)
                                (list (cons 'x   (go:type:id 'int 'int))
                                      (cons 'err (go:type:id 'error 'error)))))
                              #f))
                            "func () (x int, err error)")
                           (check-equal?
                            (emit-type
                             (go:type
                              #f
                              (go:type:id
                               'func
                               (go:type:id:func
                                (list (go:type:id 'string 'string)
                                      (go:type:id 'int 'int))
                                (list (go:type:id 'int 'int)
                                      (go:type:id 'error 'error))))
                              #f))
                            "func (string, int) (int, error)"))

	       (test-suite "create"
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'int 'int) (go:expr 0)))
                            "int(0)")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'int64 'int64) (go:expr 666)))
                            "int64(666)")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'X 'X) null))
                            "X{}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'X 'X) (go:expr 'nil)))
                            "X(nil)")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'X 'X) (list (go:expr 'nil))))
                            "X{\nnil,\n}")
			   (check-equal?
                            (emit-create
                             (go:create (go:type:id 'slice (go:type:id:slice (go:type:id 'X 'X)))
					(list (go:expr 1) (go:expr 2)
					      (go:expr 3) (go:expr 4))))
                            "[]X{\n1,\n2,\n3,\n4,\n}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'array (go:type:id:array (go:type:id 'X 'X) 4))
                                        (list (go:expr 1) (go:expr 2)
                                              (go:expr 3) (go:expr 4))))
                            "[4]X{\n1,\n2,\n3,\n4,\n}")
                           (check-equal?
                            (emit-create (go:create (go:ref (go:type:id 'X 'X)) (go:expr 'nil)))
                            "&X(nil)")
                           (check-equal?
                            (emit-create (go:create (go:deref (go:type:id 'X 'X)) (go:expr 'nil)))
                            "*X(nil)")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id
                                         'map
                                         (go:type:id:map (go:type:id 'string 'string)
                                                         (go:type:id 'int 'int)))
                                        (list (cons "1" (go:expr 1))
                                              (cons "2" (go:expr 2))
                                              (cons "3" (go:expr 3))
                                              (cons "4" (go:expr 4)))))
                            "map[string]int{\n\"1\": 1,\n\"2\": 2,\n\"3\": 3,\n\"4\": 4,\n}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id
                                         'map
                                         (go:type:id:map (go:type:id 'int 'int)
                                                         (go:type:id 'string 'string)))
                                        (list (cons 1 (go:expr "1"))
                                              (cons 2 (go:expr "2"))
                                              (cons 3 (go:expr "3"))
                                              (cons 4 (go:expr "4")))))
                            "map[int]string{\n1: \"1\",\n2: \"2\",\n3: \"3\",\n4: \"4\",\n}")
                           (check-equal?
                            (emit-create
                             (go:create (go:type:id 'struct
                                                    (go:type:id:struct
                                                     (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                        (list (go:expr 1))))
                            "struct{\n\tx int\n}{\n1,\n}")
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
                                           (go:type:id 'string 'string)
                                           (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))))
                              (list (cons "1" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                                        (list (cons 'x (go:expr 1))))))
                                    (cons "2" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                                        (list (cons 'x (go:expr 2))))))
                                    (cons "3" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                                        (list (cons 'x (go:expr 3))))))
                                    (cons "4" (go:expr (go:create
                                                        (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                                        (list (cons 'x (go:expr 4)))))))))
                            "map[string]struct{\n\tx int\n}{\n\"1\": struct{\n\tx int\n}{\nx: 1,\n},\n\"2\": struct{\n\tx int\n}{\nx: 2,\n},\n\"3\": struct{\n\tx int\n}{\nx: 3,\n},\n\"4\": struct{\n\tx int\n}{\nx: 4,\n},\n}"))

               (test-suite "def"
                           (check-equal?
                            (emit-def (go:def (list (go:expr 'x))
                                              (list (go:expr 1))))
                            "x := 1")
                           (check-equal?
                            (emit-def (go:def (list (go:expr 'x) (go:expr 'y))
                                              (list (go:expr 1)  (go:expr 2))))
                            "x, y := 1, 2")
                           (check-equal?
                            (emit-def (go:def (list (go:expr 'x))
                                              (list (go:expr (go:func (cons #f #f) #f null null null)))))
                            "x := func () () {}")
                           (check-equal?
                            (emit-def (go:def (list (go:expr 'x))
                                              (list (go:expr (go:create
                                                              (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                                                              null)))))
                            "x := []int{}"))

               (test-suite "set"
                           (check-equal?
                            (emit-set (go:set (list (go:expr 'x))
                                              (list (go:expr 1))))
                            "x = 1")
                           (check-equal?
                            (emit-set (go:set (list (go:expr 'x))
                                              (list (go:expr (go:func (cons #f #f) #f null null null)))))
                            "x = func () () {}")
                           (check-equal?
                            (emit-set (go:set (list (go:expr 'x))
                                              (list (go:expr (go:create
                                                              (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                                                              null)))))
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
                                                        +new-line+ +tab+ "\"foo\""
                                                        +new-line+ +tab+ "\"bar\""
                                                        +new-line+
                                                        +rbracket+))
                           (check-equal? (emit-imports (go:imports
                                                        (list
                                                         (go:import 'foo #f)
                                                         (go:import 'bar 'baz))))
                                         (string-append "import" +space+
                                                        +lbracket+
                                                        +new-line+ +tab+ "\"foo\""
                                                        +new-line+ +tab+ "baz" +space+ "\"bar\""
                                                        +new-line+
                                                        +rbracket+)))

               (test-suite "func"
                           (check-equal?
                            (emit-func 'Foo)
                            "Foo")
                           (check-equal?
                            (emit-func (go:func (cons #f #f) #f null null null))
                            "func () () {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f) 'hello null null null))
                            "func hello () () {}")
                           (check-equal?
                            (emit-func (go:func (cons (go:type:id 'ptr (go:type:id:ptr (go:type:id 'Struct 'Struct))) 's)
                                                'hello
                                                null null null))
                            "func (s *Struct) hello () () {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f) #f
                                                (list (go:type:id 't 't))
                                                null null))
                            "func (t) () {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f) #f
                                                (list (go:type:id
                                                       'slice
                                                       (go:type:id:slice (go:type:id 't 't))))
                                                null null))
                            "func ([]t) () {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f) #f
                                                null
                                                (list (go:type:id
                                                       'slice
                                                       (go:type:id:slice (go:type:id 't 't))))
                                                null))
                            "func () ([]t) {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f)
                                                #f
                                                `((name        . ,(go:type:id 'type 'type)))
                                                `((returnName  . ,(go:type:id 'returnType 'returnType)))
                                                null))
                            "func (name type) (returnName returnType) {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f) #f
                                                (list (go:func:type:variadic (go:type:id 't 't)))
                                                null null))
                            "func (...t) () {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f)
                                                #f
                                                `((name . ,(go:func:type:variadic (go:type:id 'type 'type))))
                                                null null))
                            "func (name ...type) () {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f)
                                                #f
                                                `((name         . ,(go:type:id 'type 'type))
                                                  (name1        . ,(go:type:id 'type1 'type1)))
                                                `((returnName   . ,(go:type:id 'returnType 'returnType))
                                                  (returnName1  . ,(go:type:id 'returnType1 'returnType1)))
                                                null))
                            "func (name type, name1 type1) (returnName returnType, returnName1 returnType1) {}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f)
                                                #f
                                                `((name       . ,(go:type:id 'type 'type)))
                                                `((returnName . ,(go:type:id 'returnType 'returnType)))
                                                (list (go:expr
                                                       (go:func (cons #f #f)
                                                                #f
                                                                `((name1        . ,(go:type:id 'type1 'type1)))
                                                                `((returnName1  . ,(go:type:id 'returnType1 'returnType1)))
                                                                null)))))
                            "func (name type) (returnName returnType) {\n\tfunc (name1 type1) (returnName1 returnType1) {}\n}")
                           (check-equal?
                            (emit-func (go:func (cons #f #f)
                                                #f
                                                `((name        . ,(go:type:id 'type 'type)))
                                                `((returnName  . ,(go:type:id 'returnType 'returnType)))
                                                (list (go:expr
                                                       (go:func (cons #f #f)
                                                                #f
                                                                `((name1        . ,(go:type:id 'type1 'type1)))
                                                                `((returnName1  . ,(go:type:id 'returnType1 'returnType1)))
                                                                null))
                                                      (go:expr
                                                       (go:func (cons #f #f)
                                                                #f
                                                                `((name1       . ,(go:type:id 'type1 'type1)))
                                                                `((returnName1 . ,(go:type:id 'returnType1 'returnType1)))
                                                                null)))))
                            "func (name type) (returnName returnType) {\n\tfunc (name1 type1) (returnName1 returnType1) {}\n\tfunc (name1 type1) (returnName1 returnType1) {}\n}"))

               (test-suite "var"
                           (check-equal?
                            (emit-var (go:var (list (go:var:binding 'x (go:type:id 'y 'y) #f))))
                            (string-append "var" +space+ +lbracket+
                                           +new-line+ +tab+ "x y"
                                           +new-line+ +rbracket+))
                           (check-equal?
                            (emit-var (go:var (list (go:var:binding 'x (go:type:id 'y 'y) (go:expr 1)))))
                            (string-append "var" +space+ +lbracket+
                                           +new-line+ +tab+ "x y = 1"
                                           +new-line+ +rbracket+))
                           (check-equal?
                            (emit-var (go:var (list (go:var:binding 'x  (go:type:id 'y 'y)  (go:expr 1))
                                                    (go:var:binding 'xx (go:type:id 'yy 'yy) (go:expr 2)))))
                            (string-append "var" +space+ +lbracket+
                                           +new-line+ +tab+ "x y = 1"
                                           +new-line+ +tab+ "xx yy = 2"
                                           +new-line+ +rbracket+)))

               (test-suite "const"
                           (check-equal?
                            (emit-const (go:const (list (go:var:binding 'x (go:type:id 'y 'y) #f))))
                            (string-append "const" +space+ +lbracket+
                                           +new-line+ +tab+ "x y"
                                           +new-line+ +rbracket+))
                           (check-equal?
                            (emit-const (go:const (list (go:var:binding 'x (go:type:id 'y 'y) (go:expr 1)))))
                            (string-append "const" +space+ +lbracket+
                                           +new-line+ +tab+ "x y = 1"
                                           +new-line+ +rbracket+))
                           (check-equal?
                            (emit-const (go:const (list (go:var:binding 'x  (go:type:id 'y 'y)  (go:expr 1))
                                                        (go:var:binding 'xx (go:type:id 'yy 'yy) (go:expr 2)))))
                            (string-append "const" +space+ +lbracket+
                                           +new-line+ +tab+ "x y = 1"
                                           +new-line+ +tab+ "xx yy = 2"
                                           +new-line+ +rbracket+)))

               (test-suite "go"
                           (check-equal?
                            (emit-go (go:go (go:expr (go:func (cons #f #f) #f null null null))))
                            "go func () () {}")
                           (check-equal?
                            (emit-go (go:go (go:expr (go:func:call (go:func (cons #f #f) #f null null null)
                                                                   null))))
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
                            "if (!((1 + 5) == 1)) {\n\tfmt.Println(\"ok\")\n} else {\n\tfmt.Println(\"not ok\")\n}")
                           (check-equal?
                            (emit-if (go:if
                                      (go:expr (go:operator '== (list (go:expr 1) (go:expr 1))))
                                      (go:begin (list (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))))
                                      #f))
                            "if (1 == 1) {\n\tfmt.Println(\"ok\")\n}")
                           (check-equal?
                            (emit-if (go:if
                                      (go:expr (go:operator '== (list (go:expr 1) (go:expr 1))))
                                      (go:expr
                                       (go:begin (list (go:expr (go:func:call 'fmt.Println (list (go:expr "ok")))))))
                                      #f))
                            "if (1 == 1) {\n\tfmt.Println(\"ok\")\n}")
                           (check-equal?
                            (emit-if (go:if
                                      (go:expr #t)
                                      (go:begin
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 1))))
                                             (go:expr (go:func:call 'fmt.Println (list (go:expr 2))))))
                                      #f))
                            "if true {\n\tfmt.Println(1)\nfmt.Println(2)\n}")
                           (check-equal?
                            (emit-if (go:if
                                      (go:operator '! (list (go:expr #t)))
                                      (go:begin
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 1))))
                                             (go:expr (go:func:call 'fmt.Println (list (go:expr 2))))))
                                      #f))
                            "if (!true) {\n\tfmt.Println(1)\nfmt.Println(2)\n}"))

               (test-suite "alias"
                           (check-equal?
                            (emit-alias (go:alias "errors" '("New" "Errorf")))
                            "var (\n\tNew = errors.New\n\tErrorf = errors.Errorf\n)")
                           (check-equal?
                            (emit-alias (go:alias "errors" '("New" ("e" . "Errorf"))))
                            "var (\n\tNew = errors.New\n\te = errors.Errorf\n)")
                           (check-equal?
                            (emit-alias (go:alias "errors" '(("NewFailure" . "Error"))))
                            "var (\n\tNewFailure = errors.Error\n)")
                           (check-equal?
                            (emit-alias (go:alias "xxx" '("Foo" "Bar" ("yyyBaz" . "Qux"))))
                            "var (\n\tFoo = xxx.Foo\n\tBar = xxx.Bar\n\tyyyBaz = xxx.Qux\n)")
                           (check-equal?
                            (emit-alias (go:alias "xxx" '("Foo" "Bar" ("Bazyyy" . "Baz") ("Quxyyy" . "Qux"))))
                            "var (\n\tFoo = xxx.Foo\n\tBar = xxx.Bar\n\tBazyyy = xxx.Baz\n\tQuxyyy = xxx.Qux\n)")
                           (check-equal?
                            (emit-alias (go:alias "xxx" (list
                                                         (go:alias:const "Foo")
                                                         (go:alias:const "Bar"))))
                            "const (\n\tFoo = xxx.Foo\n\tBar = xxx.Bar\n)")
                           (check-equal?
                            (emit-alias (go:alias "xxx" (list
                                                         (go:alias:type "Foo")
                                                         (go:alias:type "Bar"))))
                            "type (\n\tFoo = xxx.Foo\n\tBar = xxx.Bar\n)")
                           (check-equal?
                            (emit-alias (go:alias "xxx" (list '("xy" . "y")
                                                              '("xz" . "z")
                                                              (go:alias:type "Foo")
                                                              (go:alias:type "Bar"))))
                            "var (\n\txy = xxx.y\n\txz = xxx.z\n)\ntype (\n\tFoo = xxx.Foo\n\tBar = xxx.Bar\n)")
                           (check-equal?
                            (emit-alias (go:alias "xxx" (list
                                                         (go:alias:type "Bar")
                                                         (go:alias:type '("foo" . "Foo")))))
                            "type (\n\tBar = xxx.Bar\n\tfoo = xxx.Foo\n)")
                           (check-equal?
                            (emit-alias (go:alias "xxx" (list
                                                         (go:alias:type '("xfoo" . "foo"))
                                                         (go:alias:type '("xFoo" . "Foo"))
                                                         (go:alias:type '("yBar" . "Bar")))))
                            "type (\n\txfoo = xxx.foo\n\txFoo = xxx.Foo\n\tyBar = xxx.Bar\n)"))

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
                                               (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                                               (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                                       #f #f #f
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                            "for k, v := []int{\n1,\n2,\n3,\n} {\n\tfmt.Println(k, v)\n}")
                           (check-equal?
                            (emit-for (go:for
                                       (list 'k 'v)
                                       (list (go:expr
                                              (go:create
                                               (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                                               (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                                       #f #f 'range
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                            "for k, v := range []int{\n1,\n2,\n3,\n} {\n\tfmt.Println(k, v)\n}")
                           (check-equal?
                            (emit-for (go:for
                                       (list 'k)
                                       (list (go:expr 10))
                                       (go:expr (go:operator '> (list (go:expr 'k) (go:expr 0))))
                                       (go:expr (go:dec 'k))
                                       #f
                                       (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k)))))))
                            "for k := 10; (k > 0); k-- {\n\tfmt.Println(k)\n}"))

               (test-suite "begin"
                           (check-equal?
                            (emit-begin (go:begin
                                         (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v))))
                                               (go:expr (go:operator '+ (list (go:expr 1) (go:expr 2)))))))
                            "{\nfmt.Println(k, v)\n(1 + 2)\n}"))

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
                             (go:switch null
                                        (list (go:case
                                               (go:expr 1)
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                              (go:case
                                               (go:expr 2)
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                              (go:case
                                               'default
                                               (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default")))))))))
                            "switch {case 1:\n\tfmt.Println(\"one\")\ncase 2:\n\tfmt.Println(\"two\")\ndefault:\n\tfmt.Println(\"default\")}")
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
                                     (go:expr (go:def (list (go:expr 'x))
                                                      (list (go:expr (go:receive (go:expr 'ch))))))
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
                            (emit-cast (go:cast (go:expr 'v) (go:type:id 'bool 'bool)))
                            "bool(v)")
                           (check-equal?
                            (emit-cast (go:cast (go:expr 'v) (go:cast:assert (go:type:id 'bool 'bool))))
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

               (test-suite "spread"
                           (check-equal? (emit-spread (go:spread (go:expr 'X))) "X..."))

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

               (test-suite "key"
                           (check-equal?
                            (emit-key (go:key (go:expr 'st) '(Foo)))
                            "st.Foo")
                           (check-equal?
                            (emit-key (go:key (go:expr 'st) '(Foo Bar Baz)))
                            "st.Foo.Bar.Baz"))

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
                                   (go:expr (go:func (cons #f #f) 'main null null
                                                     (list (go:expr (go:func:call 'println
                                                                                  (list (go:expr 'os.Args)))))))))
                            "package main\nimport (\n\t\"os\"\n\t\"fmt\"\n)\nfunc main () () {\n\tprintln(os.Args)\n}"))

               (test-suite "complex"
                           (test-case "cli"
                             (check-equal?
                              (go/emit (list (go:expr (go:package 'main))
                                             (go:expr (go:imports (list (go:import 'os #f)
                                                                        (go:import 'fmt #f)
                                                                        (go:import "github.com/urfave/cli/v2" 'cli))))
                                             (go:expr (go:var (list (go:var:binding
                                                                     'Flags
                                                                     (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag 'cli.Flag)))
                                                                     (go:expr (go:create
                                                                               (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag 'cli.Flag)))
                                                                               (list (go:expr (go:create
                                                                                               (go:type:id 'cli.BoolFlag 'cli.BoolFlag)
                                                                                               (list (cons 'Name  (go:expr "test"))
                                                                                                     (cons 'Usage (go:expr "test flag"))))))))))))
                                             (go:expr (go:func (cons #f #f)
                                                               'RootAction
                                                               (list (cons 'ctx (go:type:id 'ptr (go:type:id:ptr (go:type:id 'cli.Context 'cli.Context)))))
                                                               (list (go:type:id 'error 'error))
                                                               (list (go:expr
                                                                      (go:func:call 'fmt.Println
                                                                                    (list (go:expr "hello from root, test is")
                                                                                          (go:expr (go:func:call 'ctx.Bool
                                                                                                                 (list (go:expr "test"))))))))))
                                             (go:expr (go:func (cons #f #f)
                                                               'main
                                                               null null
                                                               (list (go:expr (go:def
                                                                               (list (go:expr 'app))
                                                                               (list (go:expr (go:create
                                                                                               (go:type:id 'ptr
                                                                                                           (go:type:id:ptr (go:type:id 'cli.App 'cli.App)))
                                                                                               null)))))
                                                                     (go:expr (go:set (list (go:expr 'app.Flags))
                                                                                      (list (go:expr 'Flags))))
                                                                     (go:expr (go:set (list (go:expr 'app.Action))
                                                                                      (list (go:expr 'RootAction))))
                                                                     (go:expr (go:func:call 'app.Run
                                                                                            (list (go:expr 'os.Args)))))))))
                              "package main\nimport (\n\t\"os\"\n\t\"fmt\"\n\tcli \"github.com/urfave/cli/v2\"\n)\nvar (\n\tFlags []cli.Flag = []cli.Flag{\ncli.BoolFlag{\nName: \"test\",\nUsage: \"test flag\",\n},\n}\n)\nfunc RootAction (ctx *cli.Context) (error) {\n\tfmt.Println(\"hello from root, test is\", ctx.Bool(\"test\"))\n}\nfunc main () () {\n\tapp := *cli.App{}\n\tapp.Flags = Flags\n\tapp.Action = RootAction\n\tapp.Run(os.Args)\n}"))))))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
