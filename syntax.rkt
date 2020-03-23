#lang racket/base
(require racket/base
         racket/bool
         racket/match
         syntax/parse
         racket/string
         racket/format
         racket/set
         racket/list
         racket/syntax
         "type.rkt"
         "macro.rkt"
         "tool.rkt"
         (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax
                     "type.rkt"
                     "macro.rkt"
                     "tool.rkt"))

(provide (all-from-out "type.rkt")
         (all-defined-out)
         (for-syntax (all-from-out "type.rkt")
                     (all-from-out "macro.rkt")
                     (all-defined-out)))

;;

(define +new-line+ "\n")
(define +tab+      "\t")
(define +comma+    ", ")
(define +set+      " = ")
(define +space+    " ")

(begin-for-syntax
  (define *prelude*  (make-parameter (box null)))
  (define *epilogue* (make-parameter (box null))))

;;

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

;;

(define-syntax (define-gosyntax stx)
  (syntax-parse stx
    ((_ x xs ...)
     (let* ((name+args (syntax->list #'x))
            (name (car name+args))
            (args (cdr name+args)))
       (with-syntax ((name name)
                     (go/name (format-id name "go/~a" name)))
         (with-syntax ((xx #`(go/name #,@args)))
           #`(begin
               (define-syntax xx xs ...)
               (hash-set! (*macro*)
                          (quote name)
                          (lambda caller-args
                            (eval (cons (quote go/name) caller-args) ns))))))))))

;;

(define (emit-package ast)
  (match ast
    ((go:package name)
     (format "package ~a" name))))

(define (emit-imports ast)
  (match ast
    ((go:imports imports)
     (format (string-append "import (" +new-line+ +tab+ "~a" +new-line+ ")")
             (string-join (map emit-imports imports)
                          (string-append +new-line+ +tab+))))
    ((go:import package altname)
     (string-append
      (if altname
          (string-append (*->string altname) +space+)
          "")
      (~s package)))))

(define (emit-func ast)
  (match ast
    ((go:func name i o body)
     (string-append "func" +space+
                    (*->string name)       (emit-bindings (or i null))
                    +space+                (emit-bindings (or o null))
                    +space+ "{" +new-line+ (emit (expand-macro body)) +new-line+ "}"))))

(define (emit-var ast)
  (match ast
    ((go:var bindings)
     (string-append "var" +space+ (emit-bindings bindings)))))

;;

(define (emit-bindings ast)
  (match ast
    ((and (list ast ...) (list (? go:binding?) ...))
     (string-append "(" (string-join (map emit-bindings ast) +comma+) ")"))

    ((go:binding (? false?) type _)
     (~a type))

    ((go:binding name type value)
     (string-append
      (symbol->string name)
      (or (string-append +space+ (~a type)) "")
      (or (and value
               (string-append +set+
                              (emit (expand-macro value))))
          "")))))

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

(define (emit-id ast)
  (~a ast))

(define (emit-string ast)
  (~s ast))

(define (emit-number ast)
  (~a ast))

(define (emit-expr ast)
  (displayln ast)
  (match ast
    ((go:expr xs) (emit-expr (expand-macro xs)))

    ((? go:package? ast) (emit-package  ast))
    ((? go:imports? ast) (emit-imports  ast))
    ((? go:func?    ast) (emit-func     ast))
    ((? go:expr?    ast) (emit-expr     ast))
    ((? go:var?     ast) (emit-var      ast))
    ((? go:binding? ast) (emit-bindings ast))


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

(define emit emit-expr)

;;

;; (define-syntax (: stx)
;;   (syntax-parse stx
;;     ((_ xs:Colon) #`(go:group (quasiquote #,(attribute xs.ast))))))

;;

(begin-for-syntax
  (define-syntax-class PackageName
    #:description "package name/identifier"
    #:attributes (ast)
    (pattern (~or* v:id v:keyword v:string)
             #:attr ast (*->symbol #'v)))

  (define-syntax-class Package
    #:description "current package name"
    #:attributes (ast)
    #:datum-literals (package)
    (pattern (package name:PackageName)
             #:attr ast (go:package (attribute name.ast))))

  (define-syntax-class ImportPackage
    #:description "packages enumeration"
    #:attributes (ast)
    (pattern pkg:PackageName
             #:attr ast (go:import (attribute pkg.ast) #f)))

  (define-syntax-class ImportRenamePackage
    #:description "packages enumeration with name before the package specifier"
    #:attributes (ast)
    (pattern (altname:PackageName pkg:PackageName)
             #:attr ast (go:import
                         (attribute pkg.ast)
                         (attribute altname.ast))))

  (define-syntax-class Import
    #:description "package imports"
    #:attributes (ast)
    #:datum-literals (import)
    (pattern (import (~or* v:ImportRenamePackage v:ImportPackage) ...+)
             #:attr ast (go:imports (attribute v.ast))))


  ;;;;

  (define-syntax-class FuncIO
    #:description "type to name binding"
    #:attributes (ast)
    (pattern (~or* (type:Type^ ...) ((type:Type^) ...))
             #:attr ast (attribute type.ast))
    (pattern ((name:id type:Type^) ...)
             #:attr ast (map (lambda (n t) (cons n t))
                             (syntax->list #'(name ...))
                             (attribute type.ast))))

  (define-syntax-class Func
    #:description "named function definition or lambda expression"
    #:attributes (ast)
    #:datum-literals (func)
    (pattern (func (~optional name:id #:defaults ((name (syntax #f))))
                   i:FuncIO o:FuncIO body:Expr ...)
             #:attr ast (go:func (and (syntax->datum #'name)
                                      (*->symbol #'name))
                                 (attribute i.ast)
                                 (attribute o.ast)
                                 (attribute body.ast))))

  ;;;;

  (define-syntax-class Binding
    #:description "binding (name type value?)"
    #:attributes (ast)
    (pattern (~or* (name:id type:id value:Expr)
                   (name:id type:id (~optional value:Expr #:defaults ((value (syntax #f))))))
             #:attr ast (go:binding (*->symbol #'name)
                                    (*->symbol #'type)
                                    (attribute value.ast))))

  (define-syntax-class Var
    #:description "variable definition"
    #:attributes (ast)
    #:datum-literals (var)
    (pattern (var binding:Binding ...+)
             #:attr ast (go:var (attribute binding.ast))))

;;

  (define-syntax-class TypeMap
    #:description "map type description"
    #:attributes (ast kind)
    #:datum-literals (map)
    (pattern (map k:Type^ v:Type^)
             #:attr ast (go:type:map (attribute k.ast)
                                     (attribute v.ast))
             #:attr kind 'map))

  (define-syntax-class TypeStruct
    #:description "struct type description"
    #:attributes (ast kind)
    #:datum-literals (struct)
    (pattern (struct xs:TypeStructField ...)
             #:attr ast (go:type:struct (attribute xs.ast))
             #:attr kind 'struct))
  (define-syntax-class TypeStructField
    #:description "struct type field"
    #:attributes (ast)
    (pattern (k:id v:Type^ (~optional tag:string #:defaults ((tag (syntax #f)))))
             #:attr ast (go:type:struct:field (*->symbol #'k)
                                              (attribute v.ast)
                                              (syntax->datum #'tag)))
    (pattern v:Type^
             #:attr ast (go:type:struct:field #f (attribute v.ast) #f)))

  (define-syntax-class TypeInterface
    #:description "interface type description"
    #:attributes (ast kind)
    #:datum-literals (interface)
    (pattern (interface xs:TypeInterfaceField ...)
             #:attr ast (go:type:interface (attribute xs.ast))
             #:attr kind 'interface))
  (define-syntax-class TypeInterfaceField
    #:description "interface type field"
    #:attributes (ast)
    (pattern (k:id v:Type^)
             #:attr ast (go:type:interface:field (*->symbol #'k) (attribute v.ast)))
    (pattern v:Type^
             #:attr ast (go:type:interface:field #f (attribute v.ast))))

  (define-syntax-class TypeSlice
    #:description "slice type description"
    #:attributes (ast kind)
    #:datum-literals (slice)
    (pattern (slice t:Type^)
             #:attr ast (go:type:slice (attribute t.ast))
             #:attr kind 'slice))

  (define-syntax-class TypeArray
    #:description "array type description"
    #:attributes (ast kind)
    #:datum-literals (array ...)
    (pattern (array t:Type^ (~or* size:integer size:...))
             #:attr ast (go:type:array (attribute t.ast) (syntax->datum #'size))
             #:attr kind 'array))

  (define-syntax-class TypePtr
    #:description "pointer type description"
    #:attributes (ast kind)
    #:datum-literals (ptr)
    (pattern (ptr t:Type^)
             #:attr ast (go:type:ptr (attribute t.ast))
             #:attr kind 'ptr))

  (define-syntax-class TypeChan
    #:description "chan type description"
    #:attributes (ast kind)
    #:datum-literals (chan -> <-)
    (pattern (chan (~optional (~or* direction:-> direction:<-)
                              #:defaults ((direction (syntax #f))))
                   t:Type^)
             #:attr ast  (go:type:chan (syntax->datum #'direction) (attribute t.ast))
             #:attr kind 'chan))

  (define-syntax-class TypeFunc
    #:description "func type description"
    #:attributes (ast kind)
    #:datum-literals (func)
    (pattern (func i:FuncIO o:FuncIO)
             #:attr ast (go:type:func (attribute i.ast) (attribute o.ast))
             #:attr kind 'func))

  (define-syntax-class Type*
    #:description "custom user type"
    #:attributes (kind ast)
    (pattern t:id
             #:attr ast (go:type (*->symbol #'t) #f)
             #:attr kind '*))

  (define-syntax-class Type^
    #:description "type kind description"
    #:attributes (ast)
    (pattern (~or* t:TypeMap
                   t:TypeStruct
                   t:TypeInterface
                   t:TypeSlice
                   t:TypeArray
                   t:TypePtr
                   t:TypeChan
                   t:TypeFunc
                   t:Type*)
             #:attr ast (let ((kind (attribute t.kind))
                              (ast  (attribute t.ast)))
                          (if (eq? kind '*) ast (go:type kind ast)))))
  (define-syntax-class Type
    #:description "type description"
    #:attributes (ast)
    #:datum-literals (type)
    (pattern (type t:Type^)
             #:attr ast (attribute t.ast)))

  (define-syntax-class Instance
    #:description "type instance creation"
    #:attributes (ast)
    #:datum-literals (instance nil)
    (pattern (instance t:Type^ (~or* v:expr v:nil))
             #:attr ast (go:instance (attribute t.ast) (syntax->datum #'v))))

  ;;

  (define-splicing-syntax-class Expr
    #:description "expression"
    #:attributes (ast)
    #:datum-literals (nil)
    (pattern (~or* v:Package v:Import v:Func v:Var)
             #:attr ast (go:expr (attribute v.ast)))
    (pattern (~or* v:Type v:Instance)
             #:attr ast (go:expr (attribute v.ast)))
    ;; (pattern (x:id xs:Expr ...+)
    ;;          #:attr ast (go:expr (cons (syntax->datum #'x) (attribute xs.ast))))
    (pattern (~or* v:id
                   v:boolean
                   v:number
                   v:string
                   nil)
             #:attr ast (go:expr (syntax->datum #'v)))
    ))

;;

;; XXX: one of the bad sideffect of inline literals in syntax classes
;; is that things like this now should have some wrapper which will eval it
;; I think it is good, but it is a simple function, why allow such restrictions?
;; on the other hand: things like prelude will not be complete without external executor

(define-gosyntax (package stx)
  (syntax-parse stx
    (pkg:Package (attribute pkg.ast))))

(define-gosyntax (import stx)
  (syntax-parse stx
    (import:Import (attribute import.ast))))

(define-gosyntax (func stx)
  (syntax-parse stx
    (func:Func (attribute func.ast))))

(define-gosyntax (var stx)
  (syntax-parse stx
    (var:Var (attribute var.ast))))

(define-gosyntax (prog stx)
  (syntax-parse stx
    ((_ xs:Expr ...) (attribute xs.ast))))

(define-gosyntax (prelude stx)
  (syntax-parse stx
    ((_ xs:Expr ...)
     (begin0 #'(void)
       (set-box! (*prelude*)
                 (append (unbox (*prelude*))
                         (expand-macro (attribute xs.ast))))))))

(define-gosyntax (epilogue stx)
  (syntax-parse stx
    ((_ xs:Expr ...)
     (begin0 #'(void)
       (set-box! (*epilogue*)
                 (append (unbox (*epilogue*))
                         (expand-macro (attribute xs.ast))))))))

(define-syntax (go/eval stx)
  (syntax-parse stx
    ((_ ex:Expr ...+)
     (parameterize
         ((*prelude* (box null))
          (*epilogue* (box null)))
       (let ((ast (attribute ex.ast)))
         (set! ast (expand-macro ast))
         ;; (when (not (empty? (unbox (*prelude*))))
         ;;   (let ((p (unbox (*prelude*))))
         ;;     (set! ast (cons (car ast) (append p (cdr ast))))))
         ;; (when (not (empty? (unbox (*epilogue*))))
         ;;   (set! ast (append ast (unbox (*epilogue*)))))
         (with-syntax ((ast ast))
           #'(quote ast)))))))

(define (go/string instr) (emit instr))

(module+ test
  (require rackunit)

  (check-equal? (go/eval (package foo))
                (list (go:expr (go:package 'foo))))
  (check-equal? (go/eval (package #:foo))
                (list (go:expr (go:package 'foo))))
  (check-equal? (go/eval (package "foo"))
                (list (go:expr (go:package 'foo))))

  ;; import

  (check-equal? (go/eval (import foo bar))
                (list (go:expr (go:imports
                                (list
                                 (go:import 'foo #f)
                                 (go:import 'bar #f))))))
  (check-equal? (go/eval (import #:foo bar))
                (list (go:expr (go:imports
                                (list
                                 (go:import 'foo #f)
                                 (go:import 'bar #f))))))
  (check-equal? (go/eval (import (x #:foo) bar))
                (list (go:expr (go:imports
                                (list
                                 (go:import 'foo 'x)
                                 (go:import 'bar #f))))))
  (check-equal? (go/eval (import (#:x #:foo) bar))
                (list (go:expr (go:imports
                                (list
                                 (go:import 'foo 'x)
                                 (go:import 'bar #f))))))
  (check-equal? (go/eval (import (#:x "foo") "bar"))
                (list (go:expr (go:imports
                                (list
                                 (go:import 'foo 'x)
                                 (go:import 'bar #f))))))
  ;; func

  (check-equal? (go/eval (func () ()))
                (list (go:expr (go:func #f null null null))))
  (check-equal? (go/eval (func hello () ()))
                (list (go:expr (go:func 'hello null null null))))
  (check-equal? (go/eval (func (t) ()))
                (list (go:expr (go:func #f (list (go:type 't #f)) null null))))
  (check-equal? (go/eval (func ((t)) ()))
                (list (go:expr (go:func #f (list (go:type 't #f)) null null))))
  (check-equal? (go/eval (func ((name type))
                               ((return-name return-type))))
                (list (go:expr
                       (go:func #f
                                `((name        . ,(go:type 'type        #f)))
                                `((return-name . ,(go:type 'return-type #f)))
                                null))))
  (check-equal? (go/eval (func ((name type) (name1 type1))
                               ((return-name return-type)
                                (return-name1 return-type1))))
                (list (go:expr
                       (go:func #f
                                `((name         . ,(go:type 'type         #f))
                                  (name1        . ,(go:type 'type1        #f)))
                                `((return-name  . ,(go:type 'return-type  #f))
                                  (return-name1 . ,(go:type 'return-type1 #f)))
                                null))))
  (check-equal? (go/eval (func ((name type))
                               ((return-name return-type))
                               (func ((name1 type1))
                                     ((return-name1 return-type1)))))
                (list (go:expr
                       (go:func #f
                                `((name        . ,(go:type 'type        #f)))
                                `((return-name . ,(go:type 'return-type #f)))
                                (list (go:expr
                                       (go:func #f
                                                `((name1        . ,(go:type 'type1        #f)))
                                                `((return-name1 . ,(go:type 'return-type1 #f)))
                                                null)))))))
  (check-equal? (go/eval (func ((name type))
                               ((return-name return-type))
                               (func ((name1 type1))
                                     ((return-name1 return-type1)))
                               (func ((name1 type1))
                                     ((return-name1 return-type1)))))
                (list (go:expr
                       (go:func #f
                                `((name        . ,(go:type 'type        #f)))
                                `((return-name . ,(go:type 'return-type #f)))
                                (list (go:expr
                                       (go:func #f
                                                `((name1        . ,(go:type 'type1        #f)))
                                                `((return-name1 . ,(go:type 'return-type1 #f)))
                                                null))
                                      (go:expr
                                       (go:func #f
                                                `((name1        . ,(go:type 'type1        #f)))
                                                `((return-name1 . ,(go:type 'return-type1 #f)))
                                                null)))))))

  ;; var

  (check-equal? (go/eval (var (x y)))
                (list (go:expr (go:var (list (go:binding 'x 'y #f))))))
  (check-equal? (go/eval (var (x y 1)))
                (list (go:expr (go:var (list (go:binding 'x 'y (go:expr 1)))))))
  (check-equal? (go/eval (var (x y 1) (xx yy zz)))
                (list (go:expr (go:var (list (go:binding 'x  'y  (go:expr 1))
                                             (go:binding 'xx 'yy (go:expr 'zz)))))))

  ;; type

  (check-equal? (go/eval (type X))
                (list (go:expr (go:type 'X #f))))
  (check-equal? (go/eval (type (map string string)))
                (list (go:expr (go:type 'map (go:type:map (go:type 'string #f)
                                                          (go:type 'string #f))))))
  (check-equal? (go/eval (type (map string (map int X))))
                (list (go:expr
                       (go:type 'map (go:type:map
                                      (go:type 'string #f)
                                      (go:type 'map
                                               (go:type:map
                                                (go:type 'int #f)
                                                (go:type 'X #f))))))))
  (check-equal? (go/eval (type (struct
                                 io.Reader
                                 (x (map string string))
                                 (y X))))
                (list (go:expr
                       (go:type
                        'struct
                        (go:type:struct
                         (list (go:type:struct:field #f (go:type 'io.Reader #f) #f)
                               (go:type:struct:field 'x (go:type 'map (go:type:map
                                                                       (go:type 'string #f)
                                                                       (go:type 'string #f)))
                                                     #f)
                               (go:type:struct:field 'y (go:type 'X #f) #f)))))))
  (check-equal? (go/eval
                 (type
                  (interface io.Reader
                    (x (func () ())))))
                (list (go:expr (go:type 'interface
                                        (go:type:interface
                                         (list (go:type:interface:field #f (go:type 'io.Reader #f))
                                               (go:type:interface:field 'x (go:type 'func (go:type:func null null)))))))))
  (check-equal? (go/eval
                 (type
                  (interface io.Reader
                    (x (func ((k int) (v (map int string))) (error)))
                    (y (struct
                         (x (interface))
                         (y (map bool (struct))))))))
                (list (go:expr (go:type 'interface
                                        (go:type:interface
                                         (list (go:type:interface:field #f (go:type 'io.Reader #f))
                                               (go:type:interface:field 'x (go:type 'func
                                                                                    (go:type:func
                                                                                     (list (cons 'k (go:type 'int #f))
                                                                                           (cons 'v (go:type 'map (go:type:map (go:type 'int #f)
                                                                                                                               (go:type 'string #f)))))
                                                                                     (list (go:type 'error #f)))))
                                               (go:type:interface:field 'y (go:type 'struct
                                                                                    (go:type:struct
                                                                                     (list (go:type:struct:field 'x (go:type 'interface (go:type:interface null)) #f)
                                                                                           (go:type:struct:field 'y (go:type 'map (go:type:map
                                                                                                                                   (go:type 'bool #f)
                                                                                                                                   (go:type 'struct (go:type:struct null))))
                                                                                                                 #f)))))))))))

  (check-equal? (go/eval (type (slice   string)))   (list (go:expr (go:type 'slice (go:type:slice    (go:type 'string #f))))))
  (check-equal? (go/eval (type (array   string 5))) (list (go:expr (go:type 'array (go:type:array    (go:type 'string #f) 5)))))
  (check-equal? (go/eval (type (ptr     string)))   (list (go:expr (go:type 'ptr   (go:type:ptr      (go:type 'string #f))))))
  (check-equal? (go/eval (type (chan    string)))   (list (go:expr (go:type 'chan  (go:type:chan #f  (go:type 'string #f))))))
  (check-equal? (go/eval (type (chan -> string)))   (list (go:expr (go:type 'chan  (go:type:chan '-> (go:type 'string #f))))))

  ;; instance

  (check-equal? (go/eval (instance X nil))
                (list (go:expr (go:instance (go:type 'X #f) 'nil))))

  ;; other expression cases

  (check-equal? (go/eval nil)                (list (go:expr 'nil)))
  (check-equal? (go/eval #t)                 (list (go:expr #t)))
  (check-equal? (go/eval #f)                 (list (go:expr #f)))
  (check-equal? (go/eval 666)                (list (go:expr 666)))
  (check-equal? (go/eval 666.6)              (list (go:expr 666.6)))
  (check-equal? (go/eval -666)               (list (go:expr -666)))
  (check-equal? (go/eval -666.6)             (list (go:expr -666.6)))
  (check-equal? (go/eval "hello")            (list (go:expr "hello")))
  (check-equal? (go/eval runtime.GOMAXPROCS) (list (go:expr 'runtime.GOMAXPROCS)))


  )
