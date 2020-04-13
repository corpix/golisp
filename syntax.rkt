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
     (let* ((name+args (syntax->list (syntax x)))
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
    ((and (list ast ...) (list (? go:var:binding?) ...))
     (string-append "(" (string-join (map emit-var-bindings ast) +comma+) ")"))

    ((go:var:binding (? false?) type _)
     (~a type))

    ((go:var:binding name type value)
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
    ((? go:var:binding? ast) (emit-var-bindings ast))


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
    #:datum-literals
    (+ - % * /
       ++ --
       == != > < >= <=
       not and or
       bitwise-and bitwise-or bitwise-xor
       bitwise-left-shift bitwise-right-shift)
    (pattern ((~or* op:+ op:-) xs:Expr ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:% op:* op:/) xs:Expr ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:++ op:--) xs:Expr)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:== op:!= op:> op:< op:>= op:<=) xs:Expr ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern (op:not x:Expr)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (list (attribute x.ast))))
    (pattern ((~or* op:and op:or) xs:Expr ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:bitwise-and op:bitwise-or op:bitwise-xor
                    op:bitwise-left-shift op:bitwise-right-shift)
              xs:Expr ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast))))

  ;;

  (define-syntax-class TypeIdMap
    #:description "map type description"
    #:attributes (ast name)
    #:datum-literals (map)
    (pattern (map k:TypeId v:TypeId)
             #:attr ast (go:type:id:map (attribute k.ast)
                                        (attribute v.ast))
             #:attr name 'map))

  (define-syntax-class TypeIdStruct
    #:description "struct type description"
    #:attributes (ast name)
    #:datum-literals (struct)
    (pattern (struct xs:TypeIdStructField ...)
             #:attr ast (go:type:id:struct (attribute xs.ast))
             #:attr name 'struct))

  (define-syntax-class TypeIdStructField
    #:description "struct type field"
    #:attributes (ast)
    (pattern (k:id v:TypeId (~optional tag:string #:defaults ((tag (syntax #f)))))
             #:attr ast (go:type:id:struct:field (*->symbol (syntax k))
                                                 (attribute v.ast)
                                                 (syntax->datum (syntax tag))))
    (pattern v:TypeId
             #:attr ast (go:type:id:struct:field #f (attribute v.ast) #f)))

  (define-syntax-class TypeIdInterface
    #:description "interface type description"
    #:attributes (ast name)
    #:datum-literals (interface)
    (pattern (interface xs:TypeIdInterfaceField ...)
             #:attr ast (go:type:id:interface (attribute xs.ast))
             #:attr name 'interface))

  (define-syntax-class TypeIdInterfaceField
    #:description "interface type field"
    #:attributes (ast)
    (pattern (k:id v:TypeId)
             #:attr ast (go:type:id:interface:field (*->symbol (syntax k)) (attribute v.ast)))
    (pattern v:TypeId
             #:attr ast (go:type:id:interface:field #f (attribute v.ast))))

  (define-syntax-class TypeIdSlice
    #:description "slice type description"
    #:attributes (ast name)
    #:datum-literals (slice)
    (pattern (slice t:TypeId)
             #:attr ast (go:type:id:slice (attribute t.ast))
             #:attr name 'slice))

  (define-syntax-class TypeIdArray
    #:description "array type description"
    #:attributes (ast name)
    #:datum-literals (array ...)
    (pattern (array t:TypeId (~or* size:integer size:...))
             #:attr ast (go:type:id:array (attribute t.ast) (syntax->datum (syntax size)))
             #:attr name 'array))

  (define-syntax-class TypeIdPtr
    #:description "pointer type description"
    #:attributes (ast name)
    #:datum-literals (ptr)
    (pattern (ptr t:TypeId)
             #:attr ast (go:type:id:ptr (attribute t.ast))
             #:attr name 'ptr))

  (define-syntax-class TypeIdChan
    #:description "chan type description"
    #:attributes (ast name)
    #:datum-literals (chan -> <-)
    (pattern (chan (~optional (~or* direction:-> direction:<-)
                              #:defaults ((direction (syntax #f))))
                   t:TypeId)
             #:attr ast  (go:type:id:chan (syntax->datum (syntax direction)) (attribute t.ast))
             #:attr name 'chan))

  (define-syntax-class TypeIdFunc
    #:description "func type description"
    #:attributes (ast name)
    #:datum-literals (func)
    (pattern (func ((~optional i:FuncIO #:defaults ((i (syntax null))))
                    (~optional o:FuncIO #:defaults ((o (syntax null))))))
             #:attr ast (go:type:id:func
                         (or (attribute i.ast) null)
                         (or (attribute o.ast) null))
             #:attr name 'func))

  (define-syntax-class TypeId%
    #:description "custom user type description"
    #:attributes (name ast)
    (pattern t:id
             #:attr ast  #f
             #:attr name (syntax->datum (syntax t))))

  (define-syntax-class TypeId
    #:description "type name description"
    #:attributes (ast)
    (pattern (~or* t:TypeIdMap
                   t:TypeIdStruct
                   t:TypeIdInterface
                   t:TypeIdSlice
                   t:TypeIdArray
                   t:TypeIdPtr
                   t:TypeIdChan
                   t:TypeIdFunc
                   t:TypeId%)
             #:attr ast (go:type:id (attribute t.name) (attribute t.ast))))

  (define-syntax-class Type
    #:description "type definition"
    #:attributes (ast)
    #:datum-literals (type)
    (pattern (type t:TypeId)
             #:attr ast (go:type (attribute t.ast))))

  ;;

  (define-splicing-syntax-class InstanceInit
    #:description "instance init expression"
    #:attributes (ast)
    (pattern ((~or* k:id k:string) v:Expr)
             #:attr ast (cons (syntax->datum (syntax k))
                              (attribute v.ast)))
    (pattern v:Expr #:attr ast (attribute v.ast)))

  (define-syntax-class Instance
    #:description "type instance creation"
    #:attributes (ast)
    #:datum-literals (instance)
    (pattern (instance t:TypeId xs:InstanceInit ...)
             #:attr ast (go:instance (attribute t.ast)
                                     (attribute xs.ast))))

  ;;

  (define-syntax-class Def
    #:description "variable definition with type inference"
    #:attributes (ast)
    #:datum-literals (def)
    (pattern (def k:id v:Expr)
             #:attr ast (go:def (*->symbol (syntax k)) (attribute v.ast))))


  (define-syntax-class Set
    #:description "variable value initialization"
    #:attributes (ast)
    #:datum-literals (set)
    (pattern (set k:id v:Expr)
             #:attr ast (go:set (*->symbol (syntax k)) (attribute v.ast))))

  ;;

  (define-syntax-class PackageName
    #:description "package name/identifier"
    #:attributes (ast)
    (pattern (~or* v:id v:keyword v:string)
             #:attr ast (*->symbol (syntax v))))

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


  ;;

  (define-syntax-class FuncIO
    #:description "type to name binding"
    #:attributes (ast)
    (pattern (~or* (type:TypeId ...) ((type:TypeId) ...))
             #:attr ast (attribute type.ast))
    (pattern ((name:id type:TypeId) ...)
             #:attr ast (map (lambda (n t) (cons n t))
                             (syntax->list (syntax (name ...)))
                             (attribute type.ast))))

  (define-syntax-class Func
    #:description "named function definition or lambda expression"
    #:attributes (ast)
    #:datum-literals (func)
    (pattern (func ((~optional name:id  #:defaults ((name (syntax #f))))
                    (~optional i:FuncIO #:defaults ((i (syntax null))))
                    (~optional o:FuncIO #:defaults ((o (syntax null)))))
                   body:Expr ...)
             #:attr ast (go:func (and (syntax->datum (syntax name)) (*->symbol (syntax name)))
                                 (or (attribute i.ast) null)
                                 (or (attribute o.ast) null)
                                 (attribute body.ast))))

  (define-syntax-class FuncCall
    #:description "function call"
    #:attributes (ast)
    (pattern (r:Func xs:Expr ...)
             #:attr ast (go:func:call (attribute r.ast)
                                      (attribute xs.ast)))
    (pattern (r:id xs:Expr ...)
             #:attr ast (go:func:call (syntax->datum (syntax r))
                                      (attribute xs.ast))))

  ;;

  (define-syntax-class VarBinding
    #:description "var binding name, type[, value]"
    #:attributes (ast)
    (pattern (~or* (name:id type:TypeId value:Expr)
                   (name:id type:TypeId (~optional value:Expr
                                                   #:defaults ((value (syntax #f))))))
             #:attr ast (go:var:binding (*->symbol (syntax name))
                                        (attribute type.ast)
                                        (attribute value.ast))))

  (define-syntax-class Var
    #:description "variable definition"
    #:attributes (ast)
    #:datum-literals (var)
    (pattern (var binding:VarBinding ...+)
             #:attr ast (go:var (attribute binding.ast))))

  ;;

  (define-syntax-class Go
    #:description "go routine invocation"
    #:attributes (ast)
    #:datum-literals (go)
    (pattern (go r:FuncCall)
             #:attr ast (go:go (attribute r.ast))))

  ;;

  (define-syntax-class If
    #:description "if statement"
    #:attributes (ast)
    #:datum-literals (if)
    (pattern (if condition:Expr then:Expr
                 (~optional else:Expr #:defaults ((else (syntax #f)))))
             #:attr ast (go:if (attribute condition.ast)
                               (attribute then.ast)
                               (attribute else.ast))))

  ;;

  (define-syntax-class For
    #:description "for statement"
    #:attributes (ast)
    #:datum-literals (for)
    (pattern (for (vars:ForVars seq:Expr) body:Expr ...+)
             #:attr ast (go:for (attribute vars.ast)
                                (attribute seq.ast)
                                (attribute body.ast))))

  (define-syntax-class ForVars
    #:description "for statement variable bindings"
    #:attributes (ast)
    (pattern (vars:id ...+) #:attr ast (syntax->list (syntax (vars ...))))
    (pattern vars:id        #:attr ast (list (syntax->datum (syntax vars)))))

  ;;

  (define-splicing-syntax-class Expr
    #:description "expression"
    #:attributes (ast)
    #:datum-literals (nil)
    (pattern (~or* v:Package v:Import v:Var v:Type v:Instance
                   v:Operator v:Type v:Instance v:Def v:Set
                   v:Go v:If v:For
                   v:Func)
             #:attr ast (go:expr (attribute v.ast)))
    (pattern (~or* v:id
                   v:boolean
                   v:number
                   v:string
                   nil)
             #:attr ast (go:expr (syntax->datum (syntax v))))
    (pattern v:FuncCall #:attr ast (go:expr (attribute v.ast)))))

;;

(define-gosyntax (instance stx) (syntax-parse stx (instance:Instance (attribute instance.ast))))
(define-gosyntax (def stx)      (syntax-parse stx (def:Def (attribute def.ast))))
(define-gosyntax (set stx)      (syntax-parse stx (set:Set (attribute set.ast))))

;; XXX: one of the bad sideffect of inline literals in syntax classes
;; is that things like this now should have some wrapper which will eval it
;; I think it is good, but it is a simple function, why allow such restrictions?
;; on the other hand: things like prelude will not be complete without external executor

(define-gosyntax (type stx)    (syntax-parse stx (type:Type (attribute type.ast))))
(define-gosyntax (package stx) (syntax-parse stx (pkg:Package (attribute pkg.ast))))
(define-gosyntax (import stx)  (syntax-parse stx (import:Import (attribute import.ast))))
(define-gosyntax (func stx)    (syntax-parse stx (func:Func (attribute func.ast))))
(define-gosyntax (var stx)     (syntax-parse stx (var:Var (attribute var.ast))))
(define-gosyntax (go stx)      (syntax-parse stx (go:Go (attribute go.ast))))
(define-gosyntax (if stx)      (syntax-parse stx (if:If (attribute if.ast))))
(define-gosyntax (for stx)     (syntax-parse stx (for:For (attribute for.ast))))

;;

(define-gosyntax (prog stx)
  (syntax-parse stx
    ((_ xs:Expr ...) (attribute xs.ast))))

(define-gosyntax (prelude stx)
  (syntax-parse stx
    ((_ xs:Expr ...)
     (begin0 (syntax (void))
       (set-box! (*prelude*)
                 (append (unbox (*prelude*))
                         (expand-macro (attribute xs.ast))))))))

(define-gosyntax (epilogue stx)
  (syntax-parse stx
    ((_ xs:Expr ...)
     (begin0 (syntax (void))
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
           (syntax (quote ast))))))))

(define (go/string instr) (emit instr))

(module+ test
  (require rackunit rackunit/text-ui)

  (define-syntax (test-case/operator stx)
    (syntax-parse stx
      ((_ e:expr r:expr)
       (with-syntax ((operator (car (syntax->list (syntax e)))))
         (syntax (test-case (symbol->string (quote operator))
                   (check-equal? (go/eval e)
                                 (list (go:expr (go:operator (quote operator) r))))))))))

  (for
      ((suite (list
               (test-suite "operator"
                           (test-case/operator (+  1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (+  1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (-  1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (-  1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (%  1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (%  1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (*  1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (*  1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (/  1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (/  1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (++ 1)                   (go:expr 1))
                           (test-case/operator (-- 1)                   (go:expr 1))
                           (test-case/operator (== 1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (== 1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (!= 1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (!= 1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))

                           (test-case/operator (not (== 1 2))           (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))))
                           (test-case/operator (not (== 1 2 3))         (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))

                           (test-case/operator (or (== 1 2) (== 2 2))   (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))
                                                                              (go:expr (go:operator '== (list (go:expr 2) (go:expr 2))))))
                           (test-case/operator (or (== 1 2 3))          (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))

                           (test-case/operator (and (== 1 2) (== 2 2))  (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))
                                                                              (go:expr (go:operator '== (list (go:expr 2) (go:expr 2))))))
                           (test-case/operator (and (== 1 2 3))         (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))

                           (test-case/operator (>  1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (>  1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (<  1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (<  1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (>= 1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (>= 1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (<= 1 2)                 (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (<= 1 2 3)               (list (go:expr 1) (go:expr 2) (go:expr 3)))

                           (test-case/operator (bitwise-and 1 2)           (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (bitwise-and 1 2 3)         (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (bitwise-or 1 2)            (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (bitwise-or 1 2 3)          (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (bitwise-xor 1 2)           (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (bitwise-xor 1 2 3)         (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (bitwise-left-shift 1 2)    (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (bitwise-left-shift 1 2 3)  (list (go:expr 1) (go:expr 2) (go:expr 3)))
                           (test-case/operator (bitwise-right-shift 1 2)   (list (go:expr 1) (go:expr 2)))
                           (test-case/operator (bitwise-right-shift 1 2 3) (list (go:expr 1) (go:expr 2) (go:expr 3))))

               (test-suite "type"
                           (check-equal? (go/eval (type X))
                                         (list (go:expr (go:type (go:type:id 'X #f)))))
                           (check-equal? (go/eval (type (map string string)))
                                         (list (go:expr (go:type (go:type:id 'map (go:type:id:map (go:type:id 'string #f)
                                                                                                  (go:type:id 'string #f)))))))
                           (check-equal? (go/eval (type (map string (map int X))))
                                         (list (go:expr
                                                (go:type (go:type:id 'map (go:type:id:map
                                                                           (go:type:id 'string #f)
                                                                           (go:type:id 'map
                                                                                       (go:type:id:map
                                                                                        (go:type:id 'int #f)
                                                                                        (go:type:id 'X #f)))))))))
                           (check-equal? (go/eval (type (struct io.Reader (x (map string string)) (y X))))
                                         (list (go:expr
                                                (go:type (go:type:id 'struct (go:type:id:struct
                                                                              (list (go:type:id:struct:field #f (go:type:id 'io.Reader #f) #f)
                                                                                    (go:type:id:struct:field 'x (go:type:id 'map (go:type:id:map
                                                                                                                                  (go:type:id 'string #f)
                                                                                                                                  (go:type:id 'string #f)))
                                                                                                             #f)
                                                                                    (go:type:id:struct:field 'y (go:type:id 'X #f) #f))))))))
                           (check-equal? (go/eval (type (interface io.Reader (x (func ())))))
                                         (list (go:expr (go:type (go:type:id 'interface
                                                                             (go:type:id:interface
                                                                              (list (go:type:id:interface:field #f (go:type:id 'io.Reader #f))
                                                                                    (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))))))
                           (check-equal? (go/eval (type (interface io.Reader (x (func (() ()))))))
                                         (list (go:expr (go:type (go:type:id 'interface
                                                                             (go:type:id:interface
                                                                              (list (go:type:id:interface:field #f (go:type:id 'io.Reader #f))
                                                                                    (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))))))
                           (check-equal? (go/eval
                                          (type
                                           (interface io.Reader
                                             (x (func (((k int) (v (map int string))) (error))))
                                             (y (struct
                                                  (x (interface))
                                                  (y (map bool (struct))))))))
                                         (list (go:expr (go:type (go:type:id 'interface
                                                                             (go:type:id:interface
                                                                              (list (go:type:id:interface:field #f (go:type:id 'io.Reader #f))
                                                                                    (go:type:id:interface:field
                                                                                     'x
                                                                                     (go:type:id 'func
                                                                                                 (go:type:id:func
                                                                                                  (list (cons 'k (go:type:id 'int #f))
                                                                                                        (cons 'v (go:type:id 'map (go:type:id:map (go:type:id 'int #f)
                                                                                                                                                  (go:type:id 'string #f)))))
                                                                                                  (list (go:type:id 'error #f)))))
                                                                                    (go:type:id:interface:field
                                                                                     'y
                                                                                     (go:type:id 'struct
                                                                                                 (go:type:id:struct
                                                                                                  (list (go:type:id:struct:field 'x (go:type:id 'interface (go:type:id:interface null)) #f)
                                                                                                        (go:type:id:struct:field 'y (go:type:id 'map (go:type:id:map
                                                                                                                                                      (go:type:id 'bool #f)
                                                                                                                                                      (go:type:id 'struct (go:type:id:struct null))))
                                                                                                                                 #f))))))))))))

                           (check-equal? (go/eval (type (slice   string)))   (list (go:expr (go:type (go:type:id 'slice (go:type:id:slice    (go:type:id 'string #f)))))))
                           (check-equal? (go/eval (type (array   string 5))) (list (go:expr (go:type (go:type:id 'array (go:type:id:array    (go:type:id 'string #f) 5))))))
                           (check-equal? (go/eval (type (ptr     string)))   (list (go:expr (go:type (go:type:id 'ptr   (go:type:id:ptr      (go:type:id 'string #f)))))))
                           (check-equal? (go/eval (type (chan -> string)))   (list (go:expr (go:type (go:type:id 'chan  (go:type:id:chan '-> (go:type:id 'string #f)))))))
                           (check-equal? (go/eval (type (chan <- string)))   (list (go:expr (go:type (go:type:id 'chan  (go:type:id:chan '<- (go:type:id 'string #f)))))))
                           (check-equal? (go/eval (type (chan    string)))   (list (go:expr (go:type (go:type:id 'chan  (go:type:id:chan #f  (go:type:id 'string #f)))))))
                           (check-equal? (go/eval (type (chan    (struct)))) (list (go:expr (go:type (go:type:id 'chan  (go:type:id:chan #f  (go:type:id 'struct (go:type:id:struct null)))))))))

               (test-suite "instance"
                           (check-equal? (go/eval (instance X))
                                         (list (go:expr (go:instance (go:type:id 'X #f) null))))
                           (check-equal? (go/eval (instance X nil))
                                         (list (go:expr (go:instance (go:type:id 'X #f) (list (go:expr 'nil))))))
                           (check-equal? (go/eval (instance (slice int) 1 2 3 4))
                                         (list (go:expr (go:instance (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                                     (list (go:expr 1) (go:expr 2)
                                                                           (go:expr 3) (go:expr 4))))))
                           (check-equal? (go/eval (instance (array int 4) 1 2 3 4))
                                         (list (go:expr (go:instance (go:type:id 'array (go:type:id:array (go:type:id 'int #f) 4))
                                                                     (list (go:expr 1) (go:expr 2)
                                                                           (go:expr 3) (go:expr 4))))))
                           (check-equal? (go/eval (instance (map string int)
                                                            ("1" 1) ("2" 2)
                                                            ("3" 3) ("4" 4)))
                                         (list (go:expr (go:instance (go:type:id 'map
                                                                                 (go:type:id:map (go:type:id 'string #f)
                                                                                                 (go:type:id 'int    #f)))
                                                                     (list (cons "1" (go:expr 1)) (cons "2" (go:expr 2))
                                                                           (cons "3" (go:expr 3)) (cons "4" (go:expr 4)))))))

                           (check-equal? (go/eval (instance (map string (struct (x int)))
                                                            ("1" (instance (struct (x int)) 1))
                                                            ("2" (instance (struct (x int)) 2))
                                                            ("3" (instance (struct (x int)) 3))
                                                            ("4" (instance (struct (x int)) 4))))
                                         (list (go:expr
                                                (go:instance
                                                 (go:type:id 'map
                                                             (go:type:id:map
                                                              (go:type:id 'string #f)
                                                              (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))))
                                                 (list (cons "1" (go:expr (go:instance
                                                                           (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                                           (list (go:expr 1)))))
                                                       (cons "2" (go:expr (go:instance
                                                                           (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                                           (list (go:expr 2)))))
                                                       (cons "3" (go:expr (go:instance
                                                                           (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                                           (list (go:expr 3)))))
                                                       (cons "4" (go:expr (go:instance
                                                                           (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                                           (list (go:expr 4))))))))))

                           (check-equal? (go/eval (instance (struct (x (interface)))))
                                         (list (go:expr (go:instance (go:type:id 'struct
                                                                                 (go:type:id:struct (list (go:type:id:struct:field
                                                                                                           'x
                                                                                                           (go:type:id 'interface (go:type:id:interface null))
                                                                                                           #f))))
                                                                     null)))))

               (test-suite "def"
                           (check-equal? (go/eval (def x 1))
                                         (list (go:expr (go:def 'x (go:expr 1)))))
                           (check-equal? (go/eval (def x (func ())))
                                         (list (go:expr (go:def 'x (go:expr (go:func #f null null null))))))
                           (check-equal? (go/eval (def x (instance (slice int))))
                                         (list (go:expr (go:def 'x (go:expr (go:instance
                                                                             (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                                             null)))))))

               (test-suite "set"
                           (check-equal? (go/eval (set x 1))
                                         (list (go:expr (go:set 'x (go:expr 1)))))
                           (check-equal? (go/eval (set x (func ())))
                                         (list (go:expr (go:set 'x (go:expr (go:func #f null null null))))))
                           (check-equal? (go/eval (set x (instance (slice int))))
                                         (list (go:expr (go:set 'x (go:expr (go:instance
                                                                             (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
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
                                         (list (go:expr (go:func #f (list (go:type:id 't #f)) null null))))
                           (check-equal? (go/eval (func (((t)) ())))
                                         (list (go:expr (go:func #f (list (go:type:id 't #f)) null null))))
                           (check-equal? (go/eval (func (((name type))
                                                         ((return-name return-type)))))
                                         (list (go:expr
                                                (go:func #f
                                                         `((name        . ,(go:type:id 'type        #f)))
                                                         `((return-name . ,(go:type:id 'return-type #f)))
                                                         null))))
                           (check-equal? (go/eval (func (((name type) (name1 type1))
                                                         ((return-name  return-type)
                                                          (return-name1 return-type1)))))
                                         (list (go:expr
                                                (go:func #f
                                                         `((name         . ,(go:type:id 'type         #f))
                                                           (name1        . ,(go:type:id 'type1        #f)))
                                                         `((return-name  . ,(go:type:id 'return-type  #f))
                                                           (return-name1 . ,(go:type:id 'return-type1 #f)))
                                                         null))))
                           (check-equal? (go/eval (func (((name type))
                                                         ((return-name return-type)))
                                                        (func (((name1 type1))
                                                               ((return-name1 return-type1))))))
                                         (list (go:expr
                                                (go:func #f
                                                         `((name        . ,(go:type:id 'type        #f)))
                                                         `((return-name . ,(go:type:id 'return-type #f)))
                                                         (list (go:expr
                                                                (go:func #f
                                                                         `((name1        . ,(go:type:id 'type1        #f)))
                                                                         `((return-name1 . ,(go:type:id 'return-type1 #f)))
                                                                         null)))))))
                           (check-equal? (go/eval (func (((name type))
                                                         ((return-name return-type)))
                                                        (func (((name1 type1))
                                                               ((return-name1 return-type1))))
                                                        (func (((name1 type1))
                                                               ((return-name1 return-type1))))))
                                         (list (go:expr
                                                (go:func #f
                                                         `((name        . ,(go:type:id 'type        #f)))
                                                         `((return-name . ,(go:type:id 'return-type #f)))
                                                         (list (go:expr
                                                                (go:func #f
                                                                         `((name1        . ,(go:type:id 'type1        #f)))
                                                                         `((return-name1 . ,(go:type:id 'return-type1 #f)))
                                                                         null))
                                                               (go:expr
                                                                (go:func #f
                                                                         `((name1        . ,(go:type:id 'type1        #f)))
                                                                         `((return-name1 . ,(go:type:id 'return-type1 #f)))
                                                                         null))))))))

               (test-suite "var"
                           (check-equal? (go/eval (var (x y)))
                                         (list (go:expr (go:var (list (go:var:binding 'x (go:type:id 'y #f) #f))))))
                           (check-equal? (go/eval (var (x y 1)))
                                         (list (go:expr (go:var (list (go:var:binding 'x (go:type:id 'y #f) (go:expr 1)))))))
                           (check-equal? (go/eval (var (x y 1) (xx yy zz)))
                                         (list (go:expr (go:var (list (go:var:binding 'x  (go:type:id 'y #f)  (go:expr 1))
                                                                      (go:var:binding 'xx (go:type:id 'yy #f) (go:expr 'zz))))))))

               (test-suite "go"
                           (check-equal? (go/eval (go (func ())))
                                         (list (go:expr
                                                (go:func:call 'go (list (go:expr (go:func #f null null null))))))))

               (test-suite "if"
                           (check-equal? (go/eval (if (== 1 1) (fmt.Println "ok")))
                                         (list (go:expr
                                                (go:if
                                                 (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                                                 (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                                 #f))))
                           (check-equal? (go/eval (if (== 1 1) (fmt.Println "ok") (fmt.Println "not ok")))
                                         (list (go:expr
                                                (go:if
                                                 (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                                                 (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                                 (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))))
                           (check-equal? (go/eval (if (== 1 1) (fmt.Println "ok") (fmt.Println "not ok")))
                                         (list (go:expr
                                                (go:if
                                                 (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                                                 (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                                 (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok")))))))))
               (test-suite "for"
                           (check-equal? (go/eval (for ((k v) (instance (slice int) 1 2 3)) (fmt.Println k v)))
                                         (list (go:expr (go:for
                                                         (list 'k 'v)
                                                         (go:expr
                                                          (go:instance
                                                           (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                           (list (go:expr 1) (go:expr 2) (go:expr 3))))
                                                         (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))))
                           (check-equal? (go/eval (for (k (instance (slice int) 1 2 3)) (fmt.Println k)))
                                         (list (go:expr (go:for
                                                         (list 'k)
                                                         (go:expr
                                                          (go:instance
                                                           (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                           (list (go:expr 1) (go:expr 2) (go:expr 3))))
                                                         (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k))))))))))
               ;; other expression cases

               (test-suite "dummy"
                           (check-equal? (go/eval (package main)
                                                  (import os fmt)
                                                  (func (main () ()) (println os.Args)))
                                         (list (go:expr (go:package 'main))
                                               (go:expr (go:imports (list (go:import 'os #f) (go:import 'fmt #f))))
                                               (go:expr (go:func 'main null null (list (go:expr (go:func:call 'println (list (go:expr 'os.Args))))))))))

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
                                                 (go:expr (go:var (list (go:var:binding
                                                                         'Flags
                                                                         (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag #f)))
                                                                         (go:expr (go:instance
                                                                                   (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag #f)))
                                                                                   (list (go:expr (go:instance
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
                                                                                   (go:expr (go:instance
                                                                                             (go:type:id 'ptr
                                                                                                         (go:type:id:ptr (go:type:id 'cli.App #f)))
                                                                                             null))))
                                                                         (go:expr (go:set 'app.Flags  (go:expr 'Flags)))
                                                                         (go:expr (go:set 'app.Action (go:expr 'RootAction)))
                                                                         (go:expr (go:func:call 'app.Run
                                                                                                (list (go:expr 'os.Args))))))))))))))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
