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
                    (*->string name)       (emit-var-bindings (or i null))
                    +space+                (emit-var-bindings (or o null))
                    +space+ "{" +new-line+ (emit (expand-macro body)) +new-line+ "}"))))

(define (emit-var ast)
  (match ast
    ((go:var bindings)
     (string-append "var" +space+ (emit-var-bindings bindings)))))

;;

(define (emit-var-bindings ast)
  (match ast
    ((and (list ast ...) (list (? go:var-binding?) ...))
     (string-append "(" (string-join (map emit-var-bindings ast) +comma+) ")"))

    ((go:var-binding (? false?) type _)
     (~a type))

    ((go:var-binding name type value)
     (string-append
      (symbol->string name)
      ;; FIXME: type is more complex now, so this is a bug potentially
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

    ((? go:var?     ast)     (emit-var          ast))
    ((? go:var-binding? ast) (emit-var-bindings ast))


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
  (define-syntax-class Operator
    #:description "builtin operator"
    #:attributes (ast)
    #:datum-literals (+ -
                        % * /
                        ++ --
                        == > < >= <=
                        bitwise-and bitwise-or bitwise-xor bitwise-left-shift bitwise-right-shift
                        left right)
    (pattern ((~or* op:+ op:-) xs:Expr ...+)
             #:attr ast (go:operator (*->symbol #'op)
                                     (attribute xs.ast)
                                     #f))
    (pattern ((~or* op:% op:* op:/) x:Expr xs:Expr ...+)
             #:attr ast (go:operator (*->symbol #'op)
                                     (append (list (attribute x.ast))
                                             (attribute xs.ast))
                                     #f))
    (pattern ((~or* op:++ op:--) xs:Expr
                                 (~optional (~or* direction:left direction:right)
                                            #:defaults ((direction (syntax left)))))
             #:attr ast (go:operator (*->symbol #'op)
                                     (attribute xs.ast)
                                     (list (cons 'direction (syntax->datum #'direction)))))
    (pattern ((~or* op:== op:> op:< op:>= op:<=) x:Expr xs:Expr ...+)
             #:attr ast (go:operator (*->symbol #'op)
                                     (append (list (attribute x.ast))
                                             (attribute xs.ast))
                                     #f))
    (pattern ((~or* op:bitwise-and op:bitwise-or op:bitwise-xor
                    op:bitwise-left-shift op:bitwise-right-shift)
              x:Expr xs:Expr ...+)
             #:attr ast (go:operator (*->symbol #'op)
                                     (append (list (attribute x.ast))
                                             (attribute xs.ast))
                                     #f)))

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
    (pattern (func ((~optional i:FuncIO #:defaults ((i (syntax null))))
                    (~optional o:FuncIO #:defaults ((o (syntax null))))))
             #:attr ast (go:type:func
                         (or (attribute i.ast) null)
                         (or (attribute o.ast) null))
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

  (define-splicing-syntax-class InstanceInit
    #:description "instance init expression"
    #:attributes (ast)
    (pattern ((~or* k:id k:string) v:Expr)
             #:attr ast (cons (syntax->datum #'k)
                              (attribute v.ast)))
    (pattern v:Expr #:attr ast (attribute v.ast)))

  (define-syntax-class Instance
    #:description "type instance creation"
    #:attributes (ast)
    #:datum-literals (instance)
    (pattern (instance t:Type^ xs:InstanceInit ...)
             #:attr ast (go:instance (attribute t.ast)
                                     (attribute xs.ast))))

  ;;

  (define-syntax-class Def
    #:description "variable definition with type inference"
    #:attributes (ast)
    #:datum-literals (def)
    (pattern (def k:id v:Expr)
             #:attr ast (go:def (*->symbol #'k) (attribute v.ast))))


  (define-syntax-class Set
    #:description "variable value initialization"
    #:attributes (ast)
    #:datum-literals (set)
    (pattern (set k:id v:Expr)
             #:attr ast (go:set (*->symbol #'k) (attribute v.ast))))

  ;;

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
    (pattern (func ((~optional name:id  #:defaults ((name (syntax #f))))
                    (~optional i:FuncIO #:defaults ((i (syntax null))))
                    (~optional o:FuncIO #:defaults ((o (syntax null)))))
                   body:Expr ...)
             #:attr ast (go:func (and (syntax->datum #'name) (*->symbol #'name))
                                 (or (attribute i.ast) null)
                                 (or (attribute o.ast) null)
                                 (attribute body.ast))))

  ;;;;

  (define-syntax-class VarBinding
    #:description "var binding name, type[, value]"
    #:attributes (ast)
    (pattern (~or* (name:id type:Type^ value:Expr)
                   (name:id type:Type^ (~optional value:Expr
                                                  #:defaults ((value (syntax #f))))))
             #:attr ast (go:var-binding (*->symbol #'name)
                                        (attribute type.ast)
                                        (attribute value.ast))))

  (define-syntax-class Var
    #:description "variable definition"
    #:attributes (ast)
    #:datum-literals (var)
    (pattern (var binding:VarBinding ...+)
             #:attr ast (go:var (attribute binding.ast))))

  ;;

  (define-splicing-syntax-class Expr
    #:description "expression"
    #:attributes (ast)
    #:datum-literals (nil)
    (pattern (~or* v:Package v:Import v:Func v:Var v:Type v:Instance)
             #:attr ast (go:expr (attribute v.ast)))
    (pattern (~or* v:Operator
                   v:Type v:Instance
                   v:Def  v:Set)
             #:attr ast (go:expr (attribute v.ast)))
    (pattern (~or* v:id
                   v:boolean
                   v:number
                   v:string
                   nil)
             #:attr ast (go:expr (syntax->datum #'v)))
    (pattern (x:id xs:Expr ...)
             #:attr ast (go:expr (cons (syntax->datum #'x) (attribute xs.ast))))))

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

(define-gosyntax (type stx)
  (syntax-parse stx
    (type:Type (attribute type.ast))))

(define-gosyntax (instance stx)
  (syntax-parse stx
    (instance:Instance (attribute instance.ast))))

(define-gosyntax (def stx)
  (syntax-parse stx
    (def:Def (attribute def.ast))))

(define-gosyntax (set stx)
  (syntax-parse stx
    (set:Set (attribute set.ast))))



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
  (require rackunit rackunit/text-ui)

  (for
      ((suite (list
                (test-suite "operator"
                            (test-case "+"               (check-equal? (go/eval (+ 1 2))      (list (go:expr (go:operator '+  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "+ ..."           (check-equal? (go/eval (+ 1 2 3))    (list (go:expr (go:operator '+  (list (go:expr 1) (go:expr 2) (go:expr 3)) #f)))))
                            (test-case "-"               (check-equal? (go/eval (- 1 2))      (list (go:expr (go:operator '-  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "- ..."           (check-equal? (go/eval (- 1 2 3))    (list (go:expr (go:operator '-  (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "%"               (check-equal? (go/eval (% 1 2))      (list (go:expr (go:operator '%  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "% ..."           (check-equal? (go/eval (% 1 2 3))    (list (go:expr (go:operator '%  (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "*"               (check-equal? (go/eval (* 1 2))      (list (go:expr (go:operator '*  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "* ..."           (check-equal? (go/eval (* 1 2 3))    (list (go:expr (go:operator '*  (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "/"               (check-equal? (go/eval (/ 1 2))      (list (go:expr (go:operator '/  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "/ ..."           (check-equal? (go/eval (/ 1 2 3))    (list (go:expr (go:operator '/  (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "++"              (check-equal? (go/eval (++ 1))       (list (go:expr (go:operator '++ (go:expr 1) (list (cons 'direction 'left)))))))
                            (test-case "--"              (check-equal? (go/eval (-- 1))       (list (go:expr (go:operator '-- (go:expr 1) (list (cons 'direction 'left)))))))
                            (test-case "++ direction l"  (check-equal? (go/eval (++ 1 left))  (list (go:expr (go:operator '++ (go:expr 1) (list (cons 'direction 'left)))))))
                            (test-case "-- direction l"  (check-equal? (go/eval (-- 1 left))  (list (go:expr (go:operator '-- (go:expr 1) (list (cons 'direction 'left)))))))
                            (test-case "++ direction r"  (check-equal? (go/eval (++ 1 right)) (list (go:expr (go:operator '++ (go:expr 1) (list (cons 'direction 'right)))))))
                            (test-case "-- direction r"  (check-equal? (go/eval (-- 1 right)) (list (go:expr (go:operator '-- (go:expr 1) (list (cons 'direction 'right)))))))
                            (test-case "=="              (check-equal? (go/eval (== 1 2))     (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "== ..."          (check-equal? (go/eval (== 1 2 3))   (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case ">"               (check-equal? (go/eval (> 1 2))      (list (go:expr (go:operator '>  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "> ..."           (check-equal? (go/eval (> 1 2 3))    (list (go:expr (go:operator '>  (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "<"               (check-equal? (go/eval (< 1 2))      (list (go:expr (go:operator '<  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "< ..."           (check-equal? (go/eval (< 1 2 3))    (list (go:expr (go:operator '<  (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case ">="              (check-equal? (go/eval (>= 1 2))     (list (go:expr (go:operator '>= (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case ">= ..."          (check-equal? (go/eval (>= 1 2 3))   (list (go:expr (go:operator '>= (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "<="              (check-equal? (go/eval (<= 1 2))     (list (go:expr (go:operator '<= (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "<= ..."          (check-equal? (go/eval (<= 1 2 3))   (list (go:expr (go:operator '<= (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))

                            (test-case "bitwise-and"             (check-equal? (go/eval (bitwise-and 1 2))           (list (go:expr (go:operator 'bitwise-and         (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "bitwise-and ..."         (check-equal? (go/eval (bitwise-and 1 2 3))         (list (go:expr (go:operator 'bitwise-and         (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "bitwise-or"              (check-equal? (go/eval (bitwise-or 1 2))            (list (go:expr (go:operator 'bitwise-or          (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "bitwise-or ..."          (check-equal? (go/eval (bitwise-or 1 2 3))          (list (go:expr (go:operator 'bitwise-or          (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "bitwise-xor"             (check-equal? (go/eval (bitwise-xor 1 2))           (list (go:expr (go:operator 'bitwise-xor         (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "bitwise-xor ..."         (check-equal? (go/eval (bitwise-xor 1 2 3))         (list (go:expr (go:operator 'bitwise-xor         (list (go:expr 1) (go:expr 2)  (go:expr 3)) #f)))))
                            (test-case "bitwise-left-shift"      (check-equal? (go/eval (bitwise-left-shift  1 2))   (list (go:expr (go:operator 'bitwise-left-shift  (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "bitwise-left-shift ..."  (check-equal? (go/eval (bitwise-left-shift  1 2 3)) (list (go:expr (go:operator 'bitwise-left-shift  (list (go:expr 1) (go:expr 2) (go:expr 3)) #f)))))
                            (test-case "bitwise-right-shift"     (check-equal? (go/eval (bitwise-right-shift 1 2))   (list (go:expr (go:operator 'bitwise-right-shift (list (go:expr 1) (go:expr 2)) #f)))))
                            (test-case "bitwise-right-shift ..." (check-equal? (go/eval (bitwise-right-shift 1 2 3)) (list (go:expr (go:operator 'bitwise-right-shift (list (go:expr 1) (go:expr 2) (go:expr 3)) #f))))))

                (test-suite "type"
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
                            (check-equal? (go/eval (type (struct io.Reader (x (map string string)) (y X))))
                                          (list (go:expr
                                                 (go:type 'struct (go:type:struct
                                                                   (list (go:type:struct:field #f (go:type 'io.Reader #f) #f)
                                                                         (go:type:struct:field 'x (go:type 'map (go:type:map
                                                                                                                 (go:type 'string #f)
                                                                                                                 (go:type 'string #f)))
                                                                                               #f)
                                                                         (go:type:struct:field 'y (go:type 'X #f) #f)))))))
                            (check-equal? (go/eval (type (interface io.Reader (x (func ())))))
                                          (list (go:expr (go:type 'interface
                                                                  (go:type:interface
                                                                   (list (go:type:interface:field #f (go:type 'io.Reader #f))
                                                                         (go:type:interface:field 'x (go:type 'func (go:type:func null null)))))))))
                            (check-equal? (go/eval (type (interface io.Reader (x (func (() ()))))))
                                          (list (go:expr (go:type 'interface
                                                                  (go:type:interface
                                                                   (list (go:type:interface:field #f (go:type 'io.Reader #f))
                                                                         (go:type:interface:field 'x (go:type 'func (go:type:func null null)))))))))
                            (check-equal? (go/eval
                                           (type
                                            (interface io.Reader
                                              (x (func (((k int) (v (map int string))) (error))))
                                              (y (struct
                                                   (x (interface))
                                                   (y (map bool (struct))))))))
                                          (list (go:expr (go:type 'interface
                                                                  (go:type:interface
                                                                   (list (go:type:interface:field #f (go:type 'io.Reader #f))
                                                                         (go:type:interface:field
                                                                          'x
                                                                          (go:type 'func
                                                                                   (go:type:func
                                                                                    (list (cons 'k (go:type 'int #f))
                                                                                          (cons 'v (go:type 'map (go:type:map (go:type 'int #f)
                                                                                                                              (go:type 'string #f)))))
                                                                                    (list (go:type 'error #f)))))
                                                                         (go:type:interface:field
                                                                          'y
                                                                          (go:type 'struct
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
                            (check-equal? (go/eval (type (chan -> string)))   (list (go:expr (go:type 'chan  (go:type:chan '-> (go:type 'string #f)))))))

                (test-suite "instance"
                            (check-equal? (go/eval (instance X))
                                          (list (go:expr (go:instance (go:type 'X #f) null))))
                            (check-equal? (go/eval (instance X nil))
                                          (list (go:expr (go:instance (go:type 'X #f)
                                                                      (list (go:expr 'nil))))))
                            (check-equal? (go/eval (instance (slice int) 1 2 3 4))
                                          (list (go:expr (go:instance (go:type 'slice (go:type:slice (go:type 'int #f)))
                                                                      (list (go:expr 1) (go:expr 2)
                                                                            (go:expr 3) (go:expr 4))))))
                            (check-equal? (go/eval (instance (array int 4) 1 2 3 4))
                                          (list (go:expr (go:instance (go:type 'array (go:type:array (go:type 'int #f) 4))
                                                                      (list (go:expr 1) (go:expr 2)
                                                                            (go:expr 3) (go:expr 4))))))
                            (check-equal? (go/eval (instance (map string int)
                                                             ("1" 1) ("2" 2)
                                                             ("3" 3) ("4" 4)))
                                          (list (go:expr (go:instance (go:type 'map
                                                                               (go:type:map (go:type 'string #f)
                                                                                            (go:type 'int    #f)))
                                                                      (list (cons "1" (go:expr 1)) (cons "2" (go:expr 2))
                                                                            (cons "3" (go:expr 3)) (cons "4" (go:expr 4)))))))

                            (check-equal? (go/eval (instance (map string (struct (x int)))
                                                             ("1" (instance (struct (x int)) 1)) ;; FIXME: could we have more compact syntax for this?
                                                             ("2" (instance (struct (x int)) 2))
                                                             ("3" (instance (struct (x int)) 3))
                                                             ("4" (instance (struct (x int)) 4))))
                                          (list (go:expr
                                                 (go:instance
                                                  (go:type
                                                   'map
                                                   (go:type:map
                                                    (go:type 'string #f)
                                                    (go:type 'struct (go:type:struct (list (go:type:struct:field 'x (go:type 'int #f) #f))))))
                                                  (list (cons "1" (go:expr (go:instance
                                                                            (go:type 'struct (go:type:struct (list (go:type:struct:field 'x (go:type 'int #f) #f))))
                                                                            (list (go:expr 1)))))
                                                        (cons "2" (go:expr (go:instance
                                                                            (go:type 'struct (go:type:struct (list (go:type:struct:field 'x (go:type 'int #f) #f))))
                                                                            (list (go:expr 2)))))
                                                        (cons "3" (go:expr (go:instance
                                                                            (go:type 'struct (go:type:struct (list (go:type:struct:field 'x (go:type 'int #f) #f))))
                                                                            (list (go:expr 3)))))
                                                        (cons "4" (go:expr (go:instance
                                                                            (go:type 'struct (go:type:struct (list (go:type:struct:field 'x (go:type 'int #f) #f))))
                                                                            (list (go:expr 4))))))))))

                            (check-equal? (go/eval (instance (struct (x (interface)))))
                                          (list (go:expr (go:instance (go:type 'struct
                                                                               (go:type:struct (list (go:type:struct:field
                                                                                                      'x
                                                                                                      (go:type 'interface (go:type:interface null))
                                                                                                      #f))))
                                                                      null)))))

                (test-suite "def"
                            (check-equal? (go/eval (def x 1))
                                          (list (go:expr (go:def 'x (go:expr 1)))))
                            (check-equal? (go/eval (def x (func ())))
                                          (list (go:expr (go:def 'x (go:expr (go:func #f null null null))))))
                            (check-equal? (go/eval (def x (instance (slice int))))
                                          (list (go:expr (go:def 'x (go:expr (go:instance
                                                                              (go:type 'slice (go:type:slice (go:type 'int #f)))
                                                                              null)))))))

                (test-suite "set"
                            (check-equal? (go/eval (set x 1))
                                          (list (go:expr (go:set 'x (go:expr 1)))))
                            (check-equal? (go/eval (set x (func ())))
                                          (list (go:expr (go:set 'x (go:expr (go:func #f null null null))))))
                            (check-equal? (go/eval (set x (instance (slice int))))
                                          (list (go:expr (go:set 'x (go:expr (go:instance
                                                                              (go:type 'slice (go:type:slice (go:type 'int #f)))
                                                                              null)))))))

                (test-suite "package"
                            (check-equal? (go/eval (package foo))
                                          (list (go:expr (go:package 'foo))))
                            (check-equal? (go/eval (package #:foo))
                                          (list (go:expr (go:package 'foo))))
                            (check-equal? (go/eval (package "foo"))
                                          (list (go:expr (go:package 'foo)))))

                (test-suite "import"
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
                                                           (go:import 'bar #f)))))))

                (test-suite "func"
                            (check-equal? (go/eval (func ()))
                                          (list (go:expr (go:func #f null null null))))
                            (check-equal? (go/eval (func (() ())))
                                          (list (go:expr (go:func #f null null null))))
                            (check-equal? (go/eval (func (hello () ())))
                                          (list (go:expr (go:func 'hello null null null))))
                            (check-equal? (go/eval (func ((t) ())))
                                          (list (go:expr (go:func #f (list (go:type 't #f)) null null))))
                            (check-equal? (go/eval (func (((t)) ())))
                                          (list (go:expr (go:func #f (list (go:type 't #f)) null null))))
                            (check-equal? (go/eval (func (((name type))
                                                          ((return-name return-type)))))
                                          (list (go:expr
                                                 (go:func #f
                                                          `((name        . ,(go:type 'type        #f)))
                                                          `((return-name . ,(go:type 'return-type #f)))
                                                          null))))
                            (check-equal? (go/eval (func (((name type) (name1 type1))
                                                          ((return-name return-type)
                                                           (return-name1 return-type1)))))
                                          (list (go:expr
                                                 (go:func #f
                                                          `((name         . ,(go:type 'type         #f))
                                                            (name1        . ,(go:type 'type1        #f)))
                                                          `((return-name  . ,(go:type 'return-type  #f))
                                                            (return-name1 . ,(go:type 'return-type1 #f)))
                                                          null))))
                            (check-equal? (go/eval (func (((name type))
                                                          ((return-name return-type)))
                                                         (func (((name1 type1))
                                                                ((return-name1 return-type1))))))
                                          (list (go:expr
                                                 (go:func #f
                                                          `((name        . ,(go:type 'type        #f)))
                                                          `((return-name . ,(go:type 'return-type #f)))
                                                          (list (go:expr
                                                                 (go:func #f
                                                                          `((name1        . ,(go:type 'type1        #f)))
                                                                          `((return-name1 . ,(go:type 'return-type1 #f)))
                                                                          null)))))))
                            (check-equal? (go/eval (func (((name type))
                                                          ((return-name return-type)))
                                                         (func (((name1 type1))
                                                                ((return-name1 return-type1))))
                                                         (func (((name1 type1))
                                                                ((return-name1 return-type1))))))
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
                                                                          null))))))))

                (test-suite "var"
                            (check-equal? (go/eval (var (x y)))
                                          (list (go:expr (go:var (list (go:var-binding 'x (go:type 'y #f) #f))))))
                            (check-equal? (go/eval (var (x y 1)))
                                          (list (go:expr (go:var (list (go:var-binding 'x (go:type 'y #f) (go:expr 1)))))))
                            (check-equal? (go/eval (var (x y 1) (xx yy zz)))
                                          (list (go:expr (go:var (list (go:var-binding 'x  (go:type 'y #f)  (go:expr 1))
                                                                       (go:var-binding 'xx (go:type 'yy #f) (go:expr 'zz))))))))

                ;; other expression cases

                (test-suite "primitive"
                            (test-case "nil"
                              (check-equal? (go/eval nil)                (list (go:expr 'nil))))
                            (test-case "bool"
                              (check-equal? (go/eval #t)                 (list (go:expr #t)))
                              (check-equal? (go/eval #f)                 (list (go:expr #f))))
                            (test-case "number"
                              (check-equal? (go/eval 666)                (list (go:expr 666)))
                              (check-equal? (go/eval 666.6)              (list (go:expr 666.6)))
                              (check-equal? (go/eval -666)               (list (go:expr -666)))
                              (check-equal? (go/eval -666.6)             (list (go:expr -666.6))))
                            (test-case "string"
                              (check-equal? (go/eval "hello")            (list (go:expr "hello"))))
                            (test-case "identifier"
                              (check-equal? (go/eval runtime.GOMAXPROCS) (list (go:expr 'runtime.GOMAXPROCS)))))

                (test-suite "complex"
                            (test-case "cli"
                              (check-equal? (go/eval (package main)
                                                     (import os fmt
                                                             (cli github.com/urfave/cli/v2))
                                                     (var (Flags (slice cli.Flag)
                                                                 (instance (slice cli.Flag)
                                                                           (instance cli.BoolFlag
                                                                                     (Name "test")
                                                                                     (Usage "test flag")))))
                                                     (func (RootAction ((ctx (ptr cli.Context))) (error))
                                                           (fmt.Println "hello from root, test is" (ctx.Bool "test")))
                                                     (func (main () ())
                                                           (def app (instance (ptr cli.App)))
                                                           (set app.Flags    Flags)
                                                           (set app.Action   RootAction)
                                                           (app.Run os.Args)))
                                            (list (go:expr (go:package 'main))
                                                  (go:expr (go:imports (list (go:import 'os #f)
                                                                             (go:import 'fmt #f)
                                                                             (go:import 'github.com/urfave/cli/v2 'cli))))
                                                  (go:expr (go:var (list (go:var-binding
                                                                          'Flags
                                                                          (go:type 'slice (go:type:slice (go:type 'cli.Flag #f)))
                                                                          (go:expr (go:instance
                                                                                    (go:type 'slice (go:type:slice (go:type 'cli.Flag #f)))
                                                                                    (list (go:expr (go:instance
                                                                                                    (go:type 'cli.BoolFlag #f)
                                                                                                    (list (cons 'Name  (go:expr "test"))
                                                                                                          (cons 'Usage (go:expr "test flag"))))))))))))
                                                  (go:expr (go:func 'RootAction
                                                                    (list (cons 'ctx (go:type 'ptr (go:type:ptr (go:type 'cli.Context #f)))))
                                                                    (list (go:type 'error #f))
                                                                    (list (go:expr
                                                                           (list 'fmt.Println
                                                                                 (go:expr "hello from root, test is")
                                                                                 (go:expr (list 'ctx.Bool (go:expr "test"))))))))
                                                  (go:expr (go:func 'main
                                                                    null null
                                                                    (list (go:expr (go:def 'app (go:expr (go:instance
                                                                                                          (go:type 'ptr (go:type:ptr (go:type 'cli.App #f)))
                                                                                                          null))))
                                                                          (go:expr (go:set 'app.Flags  (go:expr 'Flags)))
                                                                          (go:expr (go:set 'app.Action (go:expr 'RootAction)))
                                                                          (go:expr (list 'app.Run (go:expr 'os.Args)))))))))
                            (test-case "dummy"
                              (check-equal? (go/eval (package main)
                                                     (import os fmt)
                                                     (func (main () ()) (println os.Args)))
                                            (list (go:expr (go:package 'main))
                                                  (go:expr (go:imports (list (go:import 'os #f) (go:import 'fmt #f))))
                                                  (go:expr (go:func 'main null null (list (go:expr (list 'println (go:expr 'os.Args)))))))))))))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
