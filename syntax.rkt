#lang racket/base
(require (except-in racket/list flatten)
         racket/bool
         racket/match
         racket/string
         racket/format
         racket/set
         racket/syntax
         racket/struct
	 syntax/parse
         "tool.rkt"
         (for-syntax (except-in racket/list flatten)
                     racket/base
                     racket/match
                     racket/string
                     racket/format
                     racket/syntax
                     syntax/parse
                     "type.rkt"
                     "tool.rkt"))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;;

(define-syntax (go/define-special stx) ;; FIXME: define real special which could be reached from go code!
  (syntax-parse stx                    ;;        probably will require to move from current parser implementation (see special-class-map & Expr syntax class)
    ((_ x xs ...+)
     (let* ((name+args (syntax->list (syntax x)))
            (name (car name+args))
            (args (cdr name+args)))
       (with-syntax ((name name)
                     (go/name (format-id name "go/~a" name)))
         (with-syntax ((xx (quasisyntax (go/name (unsyntax-splicing args)))))
           (quasisyntax (define-syntax xx xs ...))))))))

(define-syntax (go/define-special-class-map stx)
  (syntax-parse stx
    ((_ (name:id class:id) ...+)
     (quasisyntax
      (begin
        (unsyntax-splicing
         (for/list ((sxs (syntax->list (syntax ((name class) ...)))))
           (let ((xs (apply cons (syntax->datum sxs))))
             (with-syntax ((id         (car xs))
                           (name:class (format-id stx "~a:~a"  (car xs) (cdr xs)))
                           (ast        (format-id stx "~a.ast" (car xs))))
               (syntax (go/define-special (id s)
                                          (syntax-parse s (name:class (attribute ast))))))))))))))

;;

(begin-for-syntax
  (define-syntax-class Operator
    #:description "builtin operator"
    #:attributes (ast)
    #:datum-literals
    (+ - % * /
       == != > < >= <=
       ! not
       && and
       \|\| or
       bitwise-and ;; | is a reader in racket, so | is problematic, not supporting & too, to make it more easy to remember until I find a more beautiful solution
       bitwise-or
       ^  bitwise-xor
       << bitwise-left-shift
       >> bitwise-right-shift)
    (pattern ((~or* op:+ op:-) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:% op:* op:/) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:== op:!= op:> op:< op:>= op:<=) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* ! not) ~! x:ExprRecur)
             #:attr ast (go:operator (*->symbol (syntax !))
                                     (list (attribute x.ast))))
    (pattern ((~or* && and) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax &&))
                                     (attribute xs.ast)))
    (pattern ((~or* \|\| or) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax \|\|))
                                     (attribute xs.ast)))
    (pattern (bitwise-and ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax &))
                                     (attribute xs.ast)))
    (pattern (bitwise-or ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax \|))
                                     (attribute xs.ast)))
    (pattern ((~or* ^ bitwise-xor) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax ^))
                                     (attribute xs.ast)))
    (pattern ((~or* << bitwise-left-shift) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax <<))
                                     (attribute xs.ast)))
    (pattern ((~or* >> bitwise-right-shift) ~! xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax >>))
                                     (attribute xs.ast))))

  ;;

  (define-syntax-class TypeIdMap
    #:description "map type description"
    #:attributes (id kind ast)
    #:datum-literals (map)
    (pattern (map ~! k:TypeId v:TypeId)
             #:attr id   (quote map)
             #:attr kind (quote complex)
             #:attr ast  (go:type:id:map (attribute k.ast)
                                         (attribute v.ast))))

  (define-syntax-class TypeIdStruct
    #:description "struct type description"
    #:attributes (id kind ast)
    #:datum-literals (struct)
    (pattern (struct ~! xs:TypeIdStruct/Field ...)
             #:attr id   (quote struct)
             #:attr kind (quote complex)
             #:attr ast  (go:type:id:struct (attribute xs.ast))))

  (define-syntax-class TypeIdStruct/Field
    #:description "struct type field"
    #:attributes (ast)
    #:datum-literals (tag)
    (pattern (k:id v:TypeId (~optional t:string #:defaults ((t (syntax #f)))))
             #:attr ast (go:type:id:struct:field
                         (*->symbol (syntax k))
                         (attribute v.ast)
                         (attribute t)))
    (pattern (k:id v:TypeId (tag (tag-key:id tag-value:string) ...+))
             #:attr ast (go:type:id:struct:field
                         (*->symbol (syntax k))
                         (attribute v.ast)
                         (string-join
                          (map (lambda (k v) (string-append (symbol->string (syntax->datum k))
                                                            ":" (~s (syntax->datum v))))
                               (attribute tag-key)
                               (attribute tag-value))
                          " ")))
    (pattern v:TypeId
             #:attr ast (go:type:id:struct:field #f (attribute v.ast) #f)))

  (define-syntax-class TypeIdInterface
    #:description "interface type description"
    #:attributes (id kind ast)
    #:datum-literals (interface)
    (pattern (interface ~! xs:TypeIdInterface/Field ...)
             #:attr id   (quote interface)
             #:attr kind (quote complex)
             #:attr ast  (go:type:id:interface (attribute xs.ast))))

  (define-syntax-class TypeIdInterface/Field
    #:description "interface type field"
    #:attributes (ast)
    (pattern (k:id v:TypeId)
             #:attr ast (go:type:id:interface:field
                         (*->symbol (syntax k))
                         (attribute v.ast)))
    (pattern v:TypeId
             #:attr ast (go:type:id:interface:field
                         #f
                         (attribute v.ast))))

  (define-syntax-class TypeIdSlice
    #:description "slice type description"
    #:attributes (id kind ast)
    #:datum-literals (slice)
    (pattern (slice ~! t:TypeId)
             #:attr id   (quote slice)
             #:attr kind (quote complex)
             #:attr ast  (go:type:id:slice (attribute t.ast))))

  (define-syntax-class TypeIdArray
    #:description "array type description"
    #:attributes (id kind ast)
    #:datum-literals (array ...)
    (pattern (array ~! t:TypeId (~or* size:integer size:...))
             #:attr id (quote array)
             #:attr kind (quote complex)
             #:attr ast (go:type:id:array (attribute t.ast) (syntax->datum (syntax size)))))

  (define-syntax-class TypeIdPtr
    #:description "pointer type description"
    #:attributes (id kind ast)
    #:datum-literals (ptr)
    (pattern (ptr ~! t:TypeId)
             #:attr id   (quote ptr)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:ptr (attribute t.ast))))

  (define-syntax-class TypeIdChan
    #:description "chan type description"
    #:attributes (id kind ast)
    #:datum-literals (chan -> <-)
    (pattern (chan ~! (~optional (~or* direction:-> direction:<-)
                                 #:defaults ((direction (syntax #f))))
                   t:TypeId)
             #:attr id   (quote chan)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:chan (syntax->datum (syntax direction)) (attribute t.ast))))

  (define-syntax-class TypeIdFunc
    #:description "func type description"
    #:attributes (id kind ast)
    #:datum-literals (func)
    (pattern (func)
             #:attr id   (quote func)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:func null null))
    (pattern (func ())
             #:attr id   (quote func)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:func null null))
    (pattern (func
              ((~optional i:FuncI #:defaults ((i (syntax #f))))
               (~optional o:FuncO #:defaults ((o (syntax #f))))))
             #:attr id   (quote func)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:func (or (attribute i.ast) null)
                                          (or (attribute o.ast) null))))

  (define-syntax-class TypeId%
    #:description "custom user type description"
    #:attributes (id kind ast)
    (pattern t:id
             #:attr id   (syntax->datum (syntax t))
             #:attr kind (quote complex)
             #:attr ast  (syntax->datum (syntax t))))

  (define-syntax-class TypeId
    #:description "type id description"
    #:attributes (kind ast)
    #:datum-literals (cast ref deref)
    (pattern (~or* t:TypeIdMap
                   t:TypeIdStruct
                   t:TypeIdInterface
                   t:TypeIdSlice
                   t:TypeIdArray
                   t:TypeIdPtr
                   t:TypeIdChan
                   t:TypeIdFunc
                   t:TypeId%)
             #:attr kind (attribute t.kind)
             #:attr ast (go:type:id (attribute t.id) (attribute t.ast)))
    (pattern (cast ~! v:TypeId)
             #:attr kind (attribute v.kind)
             #:attr ast  (attribute v.ast))
    (pattern (ref ~! v:TypeId)
             #:attr kind (attribute v.kind)
             #:attr ast  (go:ref   (attribute v.ast)))
    (pattern (deref ~! v:TypeId)
             #:attr kind (attribute v.kind)
             #:attr ast  (go:deref (attribute v.ast))))

  (define-syntax-class Type
    #:description "type definition"
    #:attributes (kind ast)
    #:datum-literals (type alias)
    (pattern (type ~! (~or*
                       t:TypeId
                       (alias?:alias name:id t:TypeId%)
                       (name:id t:TypeId))
                   ...+)
             #:attr kind (attribute t.kind)
             #:attr ast  (map
                          (lambda (name type alias?)
                            (go:type name type (and alias? #t)))
                          (attribute name)
                          (attribute t.ast)
                          (attribute alias?))))

  ;;

  (define-syntax-class CompositeTypeInitializer
    #:description "composite type initializer expression"
    #:attributes (ast)
    (pattern ((~or* k:id k:string k:number) v:ExprRecur)
             #:attr ast (cons (syntax->datum (syntax k))
                              (attribute v.ast)))
    (pattern v:ExprRecur #:attr ast (attribute v.ast)))

  (define-syntax-class Create
    #:description "initialization of the type"
    #:attributes (ast)
    #:datum-literals (create)
    (pattern (create t:TypeId)
             #:attr ast (go:create (attribute t.ast) null))
    (pattern (create t:TypeId (xs:CompositeTypeInitializer ...+))
             #:attr ast (go:create (attribute t.ast)
                                   (attribute xs.ast)))
    (pattern (create t:TypeId xs:ExprRecur)
             #:attr ast (go:create (attribute t.ast)
                                   (attribute xs.ast))))

  ;;

  (define-syntax-class Def
    #:description "variable definition and initialization with type inference"
    #:attributes (ast)
    #:datum-literals (def)
    (pattern (def k:Expr v:ExprRecur)
             #:attr ast (go:def (list (attribute k.ast))
                                (list (attribute v.ast))))
    (pattern (def (k:Expr ...+) (v:ExprRecur ...+))
             #:attr ast (go:def (attribute k.ast)
                                (attribute v.ast))))

  (define-syntax-class Set
    #:description "variable initialization"
    #:attributes (ast)
    #:datum-literals (set)
    (pattern (set k:Expr v:ExprRecur)
             #:attr ast (go:set (list (attribute k.ast))
                                (list (attribute v.ast))))
    (pattern (set (k:Expr ...+) (v:ExprRecur ...+))
             #:attr ast (go:set (attribute k.ast)
                                (attribute v.ast))))

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
    (pattern (package ~! name:PackageName)
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
    (pattern (import ~! (~or* v:ImportRenamePackage v:ImportPackage) ...+)
             #:attr ast (go:imports (attribute v.ast))))


  ;;

  (define-syntax-class FuncI
    #:description "type to name binding in function input(arguments)"
    #:attributes (ast)
    #:datum-literals (&rest)
    (pattern () #:attr ast null)
    (pattern ((~seq (~optional rest:&rest #:defaults ((rest (syntax #f))))
                    type:TypeId) ...+)
             ;; FIXME: how the fuck should I fail with error here?
             ;; #:fail-when (not (= 1 (length (filter
             ;;                                (lambda (v) (syntax->datum v))
             ;;                                (attribute rest)))))
             ;; "multiple &rest keywords"

             #:attr ast (map (lambda (rest? type)
                               (if (syntax->datum rest?)
                                   (go:func:type:variadic type)
                                   type))
                             (attribute rest)
                             (attribute type.ast)))
    (pattern (((~optional rest:&rest #:defaults ((rest (syntax #f))))
               (~optional name:id    #:defaults ((name (syntax #f))))
               type:TypeId)
              ...+)
             #:attr ast (map (lambda (rest? n t)
                               (let ((type (if (syntax->datum rest?)
                                               (go:func:type:variadic t) t)))
                                 (if (syntax->datum n)
                                     (cons n type)
                                     type)))
                             (attribute rest)
                             (attribute name)
                             (attribute type.ast))))

  (define-syntax-class FuncO
    #:description "type to name binding in function output(return arguments)"
    #:attributes (ast)
    (pattern () #:attr ast null)
    (pattern (type:TypeId ...+)
             #:attr ast (attribute type.ast))
    (pattern (((~optional name:id #:defaults ((name (syntax #f))))
               type:TypeId)
              ...+)
             #:attr ast (map (lambda (n t) (if (syntax->datum n) (cons n t) t))
                             (attribute name)
                             (attribute type.ast))))

  (define-syntax-class Func
    #:description "named function definition or lambda expression"
    #:attributes (ast)
    #:datum-literals (func)
    (pattern (func) #:attr ast (go:func (cons #f #f) #f null null null))
    (pattern (func ((~optional i:FuncI #:defaults ((i (syntax #f))))
                    (~optional o:FuncO #:defaults ((o (syntax #f)))))
                   body:ExprRecur ...)
             #:attr ast (go:func (cons #f #f) #f
                                 (or (attribute i.ast) null)
                                 (or (attribute o.ast) null)
                                 (attribute body.ast)))
    (pattern (func
              ((~or* name:id
                     (name:id ((~optional struct-binding:id
                                          #:defaults ((struct-binding (syntax #f))))
                               struct-type:TypeId)))
               (~optional i:FuncI #:defaults ((i (syntax #f))))
               (~optional o:FuncO #:defaults ((o (syntax #f)))))
              body:ExprRecur ...)
             #:attr ast (go:func (cons (attribute struct-type.ast)
                                       (attribute struct-binding))
                                 (and (syntax->datum (attribute name))
                                      (*->symbol (attribute name)))
                                 (or (attribute i.ast) null)
                                 (or (attribute o.ast) null)
                                 (attribute body.ast))))

  (define-syntax-class Macro
    #:description "named macro definition"
    #:attributes (ast)
    #:datum-literals (macro)
    (pattern (macro (name:id (~optional (args:id ...))) xs:expr ...+)
             #:attr ast (go:macro (attribute name)
                                  (or (attribute args) null)
                                  (attribute xs))))

  (define-syntax-class Quote
    #:description "expression quoting"
    #:attributes (ast)
    #:datum-literals (quote quasiquote)
    (pattern (quote xs:Expr)
             #:attr ast (go:quote (syntax->datum (attribute xs)))))

  (define-syntax-class FuncCall
    #:description "function call"
    #:attributes (ast)
    (pattern (r:id ~! xs:ExprRecur ...)
             #:attr ast (go:func:call (syntax->datum (syntax r))
                                      (attribute xs.ast)))
    (pattern (r:Func ~! xs:ExprRecur ...)
             #:attr ast (go:func:call (attribute r.ast)
                                      (attribute xs.ast)))
    (pattern (r:ExprRecur ~! xs:ExprRecur ...)
             #:attr ast (go:func:call (attribute r.ast)
                                      (attribute xs.ast))))

  ;;

  (define-syntax-class VarBinding
    #:description "var binding name, type[, value]"
    #:attributes (ast)
    (pattern (name:id type:TypeId)
             #:attr ast (go:var:binding (syntax->datum (syntax name))
                                        (attribute type.ast)
                                        #f))
    (pattern (~or* (name:id type:TypeId value:ExprRecur)
                   ((name:id (~optional type:TypeId #:defaults ((type (syntax #f)))))
                    value:ExprRecur))
             #:attr ast (go:var:binding (syntax->datum (syntax name))
                                        (attribute type.ast)
                                        (attribute value.ast))))

  (define-syntax-class Var
    #:description "variable definition"
    #:attributes (ast)
    #:datum-literals (var const)
    (pattern (var ~! binding:VarBinding ...+)
             #:attr ast (go:var (attribute binding.ast)))
    (pattern (const ~! binding:VarBinding ...+)
             #:attr ast (go:const (attribute binding.ast))))

  ;;

  (define-syntax-class Go
    #:description "go routine invocation"
    #:attributes (ast)
    #:datum-literals (go)
    (pattern (go ~! expr:ExprRecur)
             #:attr ast (go:go (attribute expr.ast))))

  ;;

  (define-syntax-class If
    #:description "if statement"
    #:attributes (ast)
    #:datum-literals (if)
    (pattern (if ~!
                 condition:ExprRecur
                 then:ExprRecur
                 (~optional else:ExprRecur #:defaults ((else (syntax #f)))))
             #:attr ast (go:if (attribute condition.ast)
                               (attribute then.ast)
                               (attribute else.ast))))

  (define-syntax-class When
    #:description "when statement"
    #:attributes (ast)
    #:datum-literals (when)
    (pattern (when ~! condition:ExprRecur body:ExprRecur ...+)
             #:attr ast (go:if (attribute condition.ast)
                               (go:begin (attribute body.ast))
                               #f)))

  (define-syntax-class Unless
    #:description "unless statement"
    #:attributes (ast)
    #:datum-literals (unless)
    (pattern (unless ~! condition:ExprRecur then:ExprRecur ...+)
             #:attr ast (go:if (go:operator (quote !) (attribute condition.ast))
                               (go:begin (attribute then.ast))
                               #f)))

  ;;

  (define (alias-builder f xs)
    (map (lambda (v) (f v)) xs))

  (define (alias-rename-builder f xs)
    (map (lambda (v)
           (let ((r (f (car v))))
             (cons (if (cons? r) (car r) r)
                   (cdr v))))
         xs))

  (define-syntax-class AliasWrap
    #:description "alias wrap expression (prefix sym xs ...) or (suffix sym xs ...)"
    #:attributes (ast builder)
    #:datum-literals (prefix suffix)
    (pattern (prefix ~! sym:AliasSym xs:AliasSyms ...+)
             #:attr ast (map
                         (lambda (vv builder)
                           (builder (lambda (v) (cons (~a (car (attribute sym.ast)) v) v))
                                    vv))
                         (attribute xs.ast)
                         (attribute xs.builder))
             #:attr builder alias-rename-builder)
    (pattern (suffix ~! sym:AliasSym xs:AliasSyms ...+)
             #:attr ast (map
                         (lambda (vv builder)
                           (builder (lambda (v) (cons (~a v (car (attribute sym.ast))) v))
                                    vv))
                         (attribute xs.ast)
                         (attribute xs.builder))
             #:attr builder alias-rename-builder))

  (define-syntax-class AliasRename
    #:description "alias rename expression (rename (new old) ...)"
    #:attributes (ast builder)
    #:datum-literals (rename)
    (pattern (rename ~! (new-name:AliasSym old-name:AliasSym) ...+)
             #:attr ast (map
                         (lambda (new old) (cons new old))
                         (flatten (attribute new-name.ast))
                         (flatten (attribute old-name.ast)))
             #:attr builder alias-rename-builder))

  (define-syntax-class AliasConst
    #:description "alias const expression (const xs ...)"
    #:attributes (ast builder)
    #:datum-literals (const)
    (pattern (const ~! (~or* xs:AliasWrap xs:AliasRename xs:AliasSyms) ...+)
             #:attr ast (map
                         (lambda (v) (go:alias:const v))
                         (flatten (attribute xs.ast)))
             #:attr builder (lambda (f xs)
                              (map (lambda (v)
                                     (go:alias:const (f (go:alias:const-sym v))))
                                   xs))))

  (define-syntax-class AliasType
    #:description "alias type expression (type xs ...)"
    #:attributes (ast builder)
    #:datum-literals (type)
    (pattern (type ~! (~or* xs:AliasWrap xs:AliasRename xs:AliasSyms) ...+)
             #:attr ast (map
                         (lambda (v) (go:alias:type v))
                         (flatten (attribute xs.ast)))
             #:attr builder (lambda (f xs)
                              (map (lambda (v)
                                     (go:alias:type (f (go:alias:type-sym v))))
                                   xs))))

  (define-syntax-class AliasSym
    #:description "alias symbolic name"
    #:attributes (ast builder)
    (pattern xs:id
             #:attr ast (list (*->string (syntax->datum (attribute xs))))
             #:attr builder alias-builder))

  (define-syntax-class AliasSyms
    #:description "list of alias symbolic names (foo bar baz ...)"
    #:attributes (ast builder)
    (pattern (~or* xs:AliasSym xs:AliasWrap xs:AliasRename xs:AliasConst xs:AliasType)
             #:attr ast     (attribute xs.ast)
             #:attr builder (attribute xs.builder)))

  (define-syntax-class Alias
    #:description "alias expression"
    #:attributes (ast)
    #:datum-literals (alias)
    (pattern (alias (~or* ns:id ns:string) xs:AliasSyms ...+)
             #:attr ast (go:alias
                         (*->string (syntax->datum (attribute ns)))
                         (flatten (attribute xs.ast))))
    (pattern (alias ((~or* ns:id ns:string) xs:AliasSyms ...+) ...+)
             #:attr ast (map
                         (lambda (namespace syms)
                           (go:alias (*->string (syntax->datum namespace))
                                     (flatten syms)))
                         (attribute ns)
                         (attribute xs.ast))))

  ;;

  (define-syntax-class For
    #:description "for statement"
    #:attributes (ast)
    #:datum-literals (for range)
    (pattern (for ~! ((~optional vars:ForVars   #:defaults ((vars (syntax #f))))
                      (~optional seq:ForSeq     #:defaults ((seq  (syntax #f))))
                      (~optional pred:ExprRecur #:defaults ((pred (syntax #f))))
                      (~optional iter:ExprRecur #:defaults ((iter (syntax #f)))))
                  body:ExprRecur ...+)
             #:attr ast (go:for (or (attribute vars.ast) null)
                                (or (attribute seq.ast)  null)
                                (attribute pred.ast)
                                (attribute iter.ast)
                                (attribute seq.kind)
                                (attribute body.ast))))

  (define-syntax-class ForVars
    #:description "for statement variable bindings"
    #:attributes (ast)
    (pattern (vars:id ...+) #:attr ast (attribute vars))
    (pattern vars:id        #:attr ast (list (syntax->datum (attribute vars)))))

  (define-splicing-syntax-class ForSeq
    #:description "for statement sequence"
    #:attributes (kind ast)
    #:datum-literals (range)
    (pattern (k:range ~! seq:ExprRecur)
             #:attr kind (attribute k)
             #:attr ast (list (attribute seq.ast)))
    (pattern seq:ExprRecur
             #:attr kind #f
             #:attr ast (list (attribute seq.ast))))

  ;;

  (define-syntax-class Begin
    #:description "for statement"
    #:attributes (ast)
    #:datum-literals (begin)
    (pattern (begin ~! exprs:ExprRecur ...+)
             #:attr ast (go:begin (attribute exprs.ast))))

  ;;

  (define-syntax-class Switch
    #:description "switch statement"
    #:attributes (ast)
    #:datum-literals (switch)
    (pattern (switch ~! value:ExprRecur cases:Case ...+)
             #:attr ast (go:switch (attribute value.ast)
                                   (attribute cases.ast))))

  (define-syntax-class Cond
    #:description "cond statement"
    #:attributes (ast)
    #:datum-literals (cond)
    (pattern (cond ~! cases:Case ...+)
             #:attr ast (go:switch null (attribute cases.ast))))

  ;;

  (define-syntax-class Select
    #:description "select statement"
    #:attributes (ast)
    #:datum-literals (select)
    (pattern (select ~! cases:Case ...)
             #:attr ast (go:select (attribute cases.ast))))

  ;;

  (define-syntax-class Case
    #:description "switch statement case"
    #:attributes (ast)
    #:datum-literals (default)
    (pattern (default ~! body:ExprRecur ...+)
             #:attr ast (go:case 'default
                                 (attribute body.ast)))
    (pattern (pred:ExprRecur body:ExprRecur ...+)
             #:attr ast (go:case (attribute pred.ast)
                                 (attribute body.ast))))

  ;;

  (define-syntax-class Cast
    #:description "type cast expression"
    #:attributes (ast)
    #:datum-literals (cast assert)
    (pattern (cast value:ExprRecur (assert type:TypeId))
             #:attr ast (go:cast (attribute value.ast)
                                 (go:cast:assert (attribute type.ast))))
    (pattern (cast value:ExprRecur type:TypeId)
             #:attr ast (go:cast (attribute value.ast)
                                 (attribute type.ast))))

  ;;

  (define-syntax-class Return
    #:description "return statement"
    #:attributes (ast)
    #:datum-literals (return)
    (pattern (return ~! values:ExprRecur ...)
             #:attr ast (go:return (attribute values.ast))))

  (define-syntax-class Break
    #:description "break statement"
    #:attributes (ast)
    #:datum-literals (break)
    (pattern (break ~! (~optional label:id #:defaults ((label (syntax #f)))))
             #:attr ast (go:break (syntax-e (syntax label)))))

  (define-syntax-class Continue
    #:description "continue statement"
    #:attributes (ast)
    #:datum-literals (continue)
    (pattern (continue ~! (~optional label:id #:defaults ((label (syntax #f)))))
             #:attr ast (go:continue (syntax-e (syntax label)))))

  (define-syntax-class Spread
    #:description "spread (value...) statement"
    #:attributes (ast)
    #:datum-literals (spread)
    (pattern (spread ~! expr:ExprRecur)
             #:attr ast (go:spread (attribute expr.ast))))

  ;;

  (define-syntax-class Label
    #:description "labeled statement"
    #:attributes (ast)
    #:datum-literals (label)
    (pattern (label ~! name:id e:ExprRecur)
             #:attr ast (go:label (syntax-e (syntax name))
                                  (attribute e.ast))))

  (define-syntax-class Goto
    #:description "goto statement"
    #:attributes (ast)
    #:datum-literals (goto)
    (pattern (goto ~! label:id)
             #:attr ast (go:goto (syntax-e (syntax label)))))

  ;;

  (define-syntax-class Iota
    #:description "iota statement"
    #:attributes (ast)
    #:datum-literals (iota)
    (pattern (iota) #:attr ast (go:iota)))

  ;;

  (define-syntax-class Defer
    #:description "defer statement"
    #:attributes (ast)
    #:datum-literals (defer)
    (pattern (defer ~! body:ExprRecur) #:attr ast (go:defer (attribute body.ast))))

  ;;

  (define-syntax-class Slice
    #:description "slicing expression"
    #:attributes (ast)
    #:datum-literals (slice)
    (pattern (slice ~! value:ExprRecur
                    start:ExprRecur
                    (~optional end:ExprRecur
                               #:defaults ((end (syntax #f)))))
             #:attr ast (go:slice (attribute value.ast)
                                  (attribute start.ast)
                                  (attribute end.ast))))

  (define-syntax-class Index
    #:description "index expression"
    #:attributes (ast)
    #:datum-literals (index)
    (pattern (index ~! value:ExprRecur key:ExprRecur)
             #:attr ast (go:index (attribute value.ast)
                                  (attribute key.ast))))

  (define-syntax-class Key
    #:description "key expression"
    #:attributes (ast)
    #:datum-literals (key)
    (pattern (key ~! object:ExprRecur k:id ...+)
             #:attr ast (go:key (attribute object.ast)
                                (attribute k))))

  ;;

  (define-syntax-class Send
    #:description "channel send expression"
    #:attributes (ast)
    #:datum-literals (send ->)
    (pattern ((~or* send ->) chan:ExprRecur value:ExprRecur)
             #:attr ast (go:send (attribute chan.ast)
                                 (attribute value.ast))))

  (define-syntax-class Receive
    #:description "channel receive expression"
    #:attributes (ast)
    #:datum-literals (receive <-)
    (pattern ((~or* receive <-) ~! chan:ExprRecur)
             #:attr ast (go:receive (attribute chan.ast))))

  ;;

  (define-syntax-class Inc
    #:description "increment identifier value"
    #:attributes (ast)
    #:datum-literals (inc ++)
    (pattern ((~or* inc ++) ~! id:id)
             #:attr ast (go:inc (*->symbol (syntax id)))))

  (define-syntax-class Dec
    #:description "decrement identifier value"
    #:attributes (ast)
    #:datum-literals (dec --)
    (pattern ((~or* dec --) ~! id:id)
             #:attr ast (go:dec (*->symbol (syntax id)))))

  ;;

  (define-syntax-class Ref
    #:description "reference (get a pointer) of the expression"
    #:attributes (ast)
    #:datum-literals (ref)
    (pattern (ref ~! ex:ExprRecur)
             #:attr ast (go:ref (attribute ex.ast))))

  (define-syntax-class Deref
    #:description "dereference (get a pointer) of the expression"
    #:attributes (ast)
    #:datum-literals (deref)
    (pattern (deref ~! ex:ExprRecur)
             #:attr ast (go:deref (attribute ex.ast))))

  ;;

  (define-syntax-class Expr
    #:description "expression"
    #:attributes (ast)
    #:datum-literals (nil)
    #:commit
    (pattern (~or* v:Operator
                   v:Package  v:Import   v:Var
                   v:Type     v:Create
                   v:Def      v:Set      v:Go
                   v:If       v:When     v:Unless v:Alias
                   v:For      v:Begin
                   v:Switch   v:Cond     v:Select
                   v:Cast     v:Return   v:Break  v:Continue
                   v:Spread
                   v:Label
                   v:Goto     v:Iota     v:Defer
                   v:Slice    v:Index    v:Key
                   v:Send     v:Receive
                   v:Inc      v:Dec
                   v:Ref      v:Deref
                   v:Func     v:Macro    v:Quote)
             #:attr ast (go:expr (attribute v.ast)))
    (pattern (~or* v:nil v:id v:boolean v:number v:string)
             #:attr ast (go:expr (syntax->datum (syntax v)))))

  (define-syntax-class ExprRecur
    #:description "recursive expression"
    #:attributes (ast)
    (pattern v:Expr #:attr ast (attribute v.ast))
    (pattern v:FuncCall ;; NOTE: FuncCall class should be the last pattern, it acts as a catch-all for sexp's
             #:attr ast (go:expr (attribute v.ast)))
    (pattern v:expr ;; NOTE: allow ast nodes to be constructed inplace
             #:attr ast (attribute v))))

;;

(go/define-special-class-map
 (package  Package)
 (import   Import)
 (var      Var)
 (type     Type)
 (create   Create)
 (def      Def)
 (set      Set)
 (go       Go)
 (if       If)
 (when     When)
 (unless   Unless)
 (alias    Alias)
 (for      For)
 (begin    Begin)
 (switch   Switch)
 (cond     Cond)
 (select   Select)
 (cast     Cast)
 (return   Return)
 (break    Break)
 (continue Continue)
 (spread   Spread)
 (label    Label)
 (goto     Goto)
 (iota     Iota)
 (defer    Defer)
 (slice    Slice)
 (index    Index)
 (key      Key)
 (send     Send)
 (receive  Receive)
 (inc      Inc)
 (dec      Dec)
 (ref      Ref)
 (deref    Deref)
 (func     Func)
 (macro    Macro)
 (quote    Quote))

;;

(define-syntax (go/expand-syntax stx)
  (syntax-parse stx
    ((_ xs:ExprRecur ...+)
     (quasisyntax (quote (unsyntax (attribute xs.ast)))))))

(module+ test
  (require rackunit
           "type.rkt")

  (define-syntax (check-equal-expr? stx)
    (syntax-parse stx
      ((_ s:expr r:expr ...+)
       (syntax (check-equal? s (list (go:expr r) ...))))))

  (define-syntax (test-case/operator stx)
    (syntax-parse stx
      ((_ op:id e:expr r:expr)
       (syntax (test-case (symbol->string (quote operator))
                 (check-equal-expr? (go/expand-syntax e)
                                    (go:operator (quote op) r)))))))

  (run-suites
   (list
    (test-suite "prefab args"
                (check-equal-expr? (go/expand-syntax (+ #s(go:expr 1) #s(go:expr 1)))
                                   (go:operator '+ (list (go:expr 1) (go:expr 1)))))
    (test-suite "operator"
                (test-case/operator + (+  1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator + (+  1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator - (-  1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator - (-  1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator % (%  1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator % (%  1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator * (*  1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator * (*  1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator / (/  1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator / (/  1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator == (== 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator == (== 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator != (!= 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator != (!= 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator ! (not (== 1 2))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))))
                (test-case/operator ! (! (== 1 2))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))))
                (test-case/operator ! (not (== 1 2 3))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))
                (test-case/operator ! (! (== 1 2 3))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))
                (test-case/operator \|\| (or (== 1 2) (== 2 2))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))
                                          (go:expr (go:operator '== (list (go:expr 2) (go:expr 2))))))
                (test-case/operator \|\| (\|\| (== 1 2) (== 2 2))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))
                                          (go:expr (go:operator '== (list (go:expr 2) (go:expr 2))))))
                (test-case/operator \|\| (or (== 1 2 3))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))
                (test-case/operator \|\| (\|\| (== 1 2 3))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))
                (test-case/operator && (and (== 1 2) (== 2 2))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))
                                          (go:expr (go:operator '== (list (go:expr 2) (go:expr 2))))))
                (test-case/operator && (&& (== 1 2) (== 2 2))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2))))
                                          (go:expr (go:operator '== (list (go:expr 2) (go:expr 2))))))
                (test-case/operator && (and (== 1 2 3))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))
                (test-case/operator && (&& (== 1 2 3))
                                    (list (go:expr (go:operator '== (list (go:expr 1) (go:expr 2) (go:expr 3))))))
                (test-case/operator > (>  1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator > (>  1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator < (<  1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator < (<  1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator >= (>= 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator >= (>= 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator <= (<= 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator <= (<= 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator & (bitwise-and 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator & (bitwise-and 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator \| (bitwise-or 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator \| (bitwise-or 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator ^ (bitwise-xor 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator ^ (^ 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator ^ (bitwise-xor 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator ^ (^ 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator << (bitwise-left-shift 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator << (<< 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator << (bitwise-left-shift 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator << (<< 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator >> (bitwise-right-shift 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator >> (>> 1 2)
                                    (list (go:expr 1) (go:expr 2)))
                (test-case/operator >> (bitwise-right-shift 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))
                (test-case/operator >> (>> 1 2 3)
                                    (list (go:expr 1) (go:expr 2) (go:expr 3))))

    (test-suite "type"
                (check-equal-expr?
                 (go/expand-syntax (type X))
                 (list (go:type #f (go:type:id 'X 'X) #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (map string string)))
                 (list (go:type #f (go:type:id 'map (go:type:id:map (go:type:id 'string 'string)
                                                                    (go:type:id 'string 'string)))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (map string (map int X))))
                 (list (go:type #f (go:type:id 'map (go:type:id:map
                                                     (go:type:id 'string 'string)
                                                     (go:type:id 'map
                                                                 (go:type:id:map
                                                                  (go:type:id 'int 'int)
                                                                  (go:type:id 'X 'X)))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (struct (X string "json:\"x\""))))
                 (list (go:type #f (go:type:id
                                    'struct
                                    (go:type:id:struct
                                     (list (go:type:id:struct:field 'X (go:type:id 'string 'string)
                                                                    "json:\"x\""))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (struct (X string (tag (json "x"))))))
                 (list (go:type #f (go:type:id
                                    'struct
                                    (go:type:id:struct
                                     (list (go:type:id:struct:field 'X (go:type:id 'string 'string)
                                                                    "json:\"x\""))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (struct io.Reader (x (map string string)) (y X))))
                 (list (go:type #f (go:type:id 'struct (go:type:id:struct
                                                        (list (go:type:id:struct:field #f (go:type:id 'io.Reader 'io.Reader) #f)
                                                              (go:type:id:struct:field 'x (go:type:id 'map (go:type:id:map
                                                                                                            (go:type:id 'string 'string)
                                                                                                            (go:type:id 'string 'string)))
                                                                                       #f)
                                                              (go:type:id:struct:field 'y (go:type:id 'X 'X) #f))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (interface (x (func)))))
                 (list (go:type #f (go:type:id 'interface
                                               (go:type:id:interface
                                                (list (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (interface (x (func ())))))
                 (list (go:type #f (go:type:id 'interface
                                               (go:type:id:interface
                                                (list (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (interface io.Reader (x (func)))))
                 (list (go:type #f (go:type:id 'interface
                                               (go:type:id:interface
                                                (list (go:type:id:interface:field #f (go:type:id 'io.Reader 'io.Reader))
                                                      (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax
                  (type
                   (interface io.Reader
                     (x (func (((k int) (v (map int string))) (error))))
                     (y (struct
                          (x (interface))
                          (y (map bool (struct))))))))
                 (list (go:type #f (go:type:id 'interface
                                               (go:type:id:interface
                                                (list (go:type:id:interface:field #f (go:type:id 'io.Reader 'io.Reader))
                                                      (go:type:id:interface:field
                                                       'x
                                                       (go:type:id 'func
                                                                   (go:type:id:func
                                                                    (list (cons 'k (go:type:id 'int 'int))
                                                                          (cons 'v (go:type:id 'map (go:type:id:map (go:type:id 'int 'int)
                                                                                                                    (go:type:id 'string 'string)))))
                                                                    (list (go:type:id 'error 'error)))))
                                                      (go:type:id:interface:field
                                                       'y
                                                       (go:type:id 'struct
                                                                   (go:type:id:struct
                                                                    (list (go:type:id:struct:field 'x (go:type:id 'interface (go:type:id:interface null)) #f)
                                                                          (go:type:id:struct:field 'y (go:type:id 'map (go:type:id:map
                                                                                                                        (go:type:id 'bool 'bool)
                                                                                                                        (go:type:id 'struct (go:type:id:struct null))))
                                                                                                   #f))))))))
                                #f)))

                (check-equal-expr?
                 (go/expand-syntax (type (slice string)))
                 (list (go:type #f (go:type:id 'slice (go:type:id:slice (go:type:id 'string 'string)))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (array string 5)))
                 (list (go:type #f (go:type:id 'array (go:type:id:array (go:type:id 'string 'string) 5))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (ptr string)))
                 (list (go:type #f (go:type:id 'ptr (go:type:id:ptr (go:type:id 'string 'string)))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (chan -> string)))
                 (list (go:type #f (go:type:id 'chan (go:type:id:chan '-> (go:type:id 'string 'string)))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (chan <- string)))
                 (list (go:type #f (go:type:id 'chan (go:type:id:chan '<- (go:type:id 'string 'string)))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (chan string)))
                 (list (go:type #f (go:type:id 'chan (go:type:id:chan #f (go:type:id 'string 'string)))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (chan (struct))))
                 (list (go:type #f (go:type:id 'chan (go:type:id:chan #f (go:type:id 'struct (go:type:id:struct null))))
                                #f)))

                (check-equal-expr?
                 (go/expand-syntax (type (func (((k string) (v int)) (int error)))))
                 (list (go:type
                        #f
                        (go:type:id
                         'func
                         (go:type:id:func
                          (list (cons 'k (go:type:id 'string 'string))
                                (cons 'v (go:type:id 'int 'int)))
                          (list (go:type:id 'int 'int)
                                (go:type:id 'error 'error))))
                        #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (func ((string int) (int error)))))
                 (list (go:type
                        #f
                        (go:type:id
                         'func
                         (go:type:id:func
                          (list (go:type:id 'string 'string)
                                (go:type:id 'int 'int))
                          (list (go:type:id 'int 'int)
                                (go:type:id 'error 'error))))
                        #f)))

                (check-equal-expr?
                 (go/expand-syntax (type (alias newName oldName)))
                 (list (go:type 'newName 'oldName #t)))
                (check-equal-expr?
                 (go/expand-syntax (type (name (chan (struct)))))
                 (list (go:type 'name
                                (go:type:id 'chan
                                            (go:type:id:chan #f
                                                             (go:type:id 'struct (go:type:id:struct null))))
                                #f)))
                (check-equal-expr?
                 (go/expand-syntax (type (name string) (label string)))
                 (list (go:type 'name
                                (go:type:id 'string 'string)
                                #f)
                       (go:type 'label
                                (go:type:id 'string 'string)
                                #f))))

    (test-suite "create"
                (check-equal-expr?
                 (go/expand-syntax (create X))
                 (go:create (go:type:id 'X 'X) null))
                (check-equal-expr?
                 (go/expand-syntax (create X nil))
                 (go:create (go:type:id 'X 'X) (go:expr 'nil)))
                (check-equal-expr?
                 (go/expand-syntax (create (cast X) nil))
                 (go:create (go:type:id 'X 'X) (go:expr 'nil)))
                (check-equal-expr?
                 (go/expand-syntax (create (ref X) nil))
                 (go:create (go:ref (go:type:id 'X 'X))
                            (go:expr 'nil)))
                (check-equal-expr?
                 (go/expand-syntax (create (deref X) nil))
                 (go:create (go:deref (go:type:id 'X 'X))
                            (go:expr 'nil)))
                (check-equal-expr?
                 (go/expand-syntax (create (slice int) (1 2 3 4)))
                 (go:create (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                            (list (go:expr 1) (go:expr 2)
                                  (go:expr 3) (go:expr 4))))
                (check-equal-expr?
                 (go/expand-syntax (create (array int 4) (1 2 3 4)))
                 (go:create (go:type:id 'array (go:type:id:array (go:type:id 'int 'int) 4))
                            (list (go:expr 1) (go:expr 2)
                                  (go:expr 3) (go:expr 4))))
                (check-equal-expr?
                 (go/expand-syntax (create (map string int)
                                           (("1" 1)
                                            ("2" 2)
                                            ("3" 3)
                                            ("4" 4))))
                 (go:create (go:type:id 'map
                                        (go:type:id:map (go:type:id 'string 'string)
                                                        (go:type:id 'int 'int)))
                            (list (cons "1" (go:expr 1)) (cons "2" (go:expr 2))
                                  (cons "3" (go:expr 3)) (cons "4" (go:expr 4)))))

                (check-equal-expr?
                 (go/expand-syntax (create (struct (x int)) (1)))
                 (go:create (go:type:id 'struct
                                        (go:type:id:struct
                                         (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                            (list (go:expr 1))))
                (check-equal-expr?
                 (go/expand-syntax (create (struct (x (interface))) ((NewReader))))
                 (go:create (go:type:id 'struct
                                        (go:type:id:struct
                                         (list (go:type:id:struct:field 'x (go:type:id 'interface (go:type:id:interface null)) #f))))
                            (list (go:expr (go:func:call 'NewReader null)))))
                (check-equal-expr?
                 (go/expand-syntax (create (struct (x (interface)))))
                 (go:create (go:type:id 'struct
                                        (go:type:id:struct (list (go:type:id:struct:field
                                                                  'x
                                                                  (go:type:id 'interface (go:type:id:interface null))
                                                                  #f))))
                            null))
                (check-equal-expr?
                 (go/expand-syntax (create (map string (struct (x int)))
                                           (("1" (create (struct (x int)) (x 1)))
                                            ("2" (create (struct (x int)) (x 2)))
                                            ("3" (create (struct (x int)) (x 3)))
                                            ("4" (create (struct (x int)) (x 4))))))
                 (go:create
                  (go:type:id 'map
                              (go:type:id:map
                               (go:type:id 'string 'string)
                               (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))))
                  (list (cons "1" (go:expr (go:create
                                            (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                            (list (go:expr 'x) (go:expr 1)))))
                        (cons "2" (go:expr (go:create
                                            (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                            (list (go:expr 'x) (go:expr 2)))))
                        (cons "3" (go:expr (go:create
                                            (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                            (list (go:expr 'x) (go:expr 3)))))
                        (cons "4" (go:expr (go:create
                                            (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int 'int) #f))))
                                            (list (go:expr 'x) (go:expr 4)))))))))

    (test-suite "def"
                (check-equal-expr?
                 (go/expand-syntax (def x 1))
                 (go:def (list (go:expr 'x)) (list (go:expr 1))))
                (check-equal-expr?
                 (go/expand-syntax (def (x y) (1 2)))
                 (go:def (list (go:expr 'x) (go:expr 'y))
                         (list (go:expr 1) (go:expr 2))))
                (check-equal-expr?
                 (go/expand-syntax (def x (func)))
                 (go:def (list (go:expr 'x))
                         (list (go:expr (go:func (cons #f #f) #f null null null)))))
                (check-equal-expr?
                 (go/expand-syntax (def x (func ())))
                 (go:def (list (go:expr 'x))
                         (list (go:expr (go:func (cons #f #f) #f null null null)))))
                (check-equal-expr?
                 (go/expand-syntax (def x (create (slice int))))
                 (go:def (list (go:expr 'x))
                         (list (go:expr (go:create
                                         (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                                         null))))))

    (test-suite "set"
                (check-equal-expr?
                 (go/expand-syntax (set x 1))
                 (go:set (list (go:expr 'x))
                         (list (go:expr 1))))
                (check-equal-expr?
                 (go/expand-syntax (set (x y) (1 2)))
                 (go:set (list (go:expr 'x) (go:expr 'y))
                         (list (go:expr 1) (go:expr 2))))
                (check-equal-expr?
                 (go/expand-syntax (set x (func)))
                 (go:set (list (go:expr 'x))
                         (list (go:expr (go:func (cons #f #f) #f null null null)))))
                (check-equal-expr?
                 (go/expand-syntax (set x (func ())))
                 (go:set (list (go:expr 'x))
                         (list (go:expr (go:func (cons #f #f) #f null null null)))))
                (check-equal-expr?
                 (go/expand-syntax (set x (create (slice int))))
                 (go:set (list (go:expr 'x))
                         (list (go:expr (go:create
                                         (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                                         null))))))

    (test-suite "package"
                (check-equal-expr?
                 (go/expand-syntax (package foo))
                 (go:package 'foo))
                (check-equal-expr?
                 (go/expand-syntax (package #:foo))
                 (go:package 'foo))
                (check-equal-expr?
                 (go/expand-syntax (package "foo"))
                 (go:package 'foo)))

    (test-suite "import"
                (check-equal-expr?
                 (go/expand-syntax (import foo bar))
                 (go:imports
                  (list
                   (go:import 'foo #f)
                   (go:import 'bar #f))))
                (check-equal-expr?
                 (go/expand-syntax (import #:foo bar))
                 (go:imports
                  (list
                   (go:import 'foo #f)
                   (go:import 'bar #f))))
                (check-equal-expr?
                 (go/expand-syntax (import (x #:foo) bar))
                 (go:imports
                  (list
                   (go:import 'foo 'x)
                   (go:import 'bar #f))))
                (check-equal-expr?
                 (go/expand-syntax (import (#:x #:foo) bar))
                 (go:imports
                  (list
                   (go:import 'foo 'x)
                   (go:import 'bar #f))))
                (check-equal-expr?
                 (go/expand-syntax (import (#:x "foo") "bar"))
                 (go:imports
                  (list
                   (go:import 'foo 'x)
                   (go:import 'bar #f)))))

    (test-suite "func"
                (check-equal-expr?
                 (go/expand-syntax (func))
                 (go:func (cons #f #f) #f null null null))
                (check-equal-expr?
                 (go/expand-syntax (func ()))
                 (go:func (cons #f #f) #f null null null))
                (check-equal-expr?
                 (go/expand-syntax (func (hello)))
                 (go:func (cons #f #f)
                          'hello null null null))
                (check-equal-expr?
                 (go/expand-syntax (func (hello ())))
                 (go:func (cons #f #f)
                          'hello null null null))
                (check-equal-expr?
                 (go/expand-syntax (func (hello () ())))
                 (go:func (cons #f #f)
                          'hello null null null))
                (check-equal-expr?
                 (go/expand-syntax (func ((hello ((ptr Struct))))))
                 (go:func (cons (go:type:id 'ptr (go:type:id:ptr (go:type:id 'Struct 'Struct)))
                                #f)
                          'hello
                          null null null))
                (check-equal-expr?
                 (go/expand-syntax (func ((hello (s (ptr Struct))))))
                 (go:func (cons (go:type:id 'ptr (go:type:id:ptr (go:type:id 'Struct 'Struct))) 's)
                          'hello
                          null null null))
                (check-equal-expr?
                 (go/expand-syntax (func ((t))))
                 (go:func (cons #f #f) #f (list (go:type:id 't 't)) null null))
                (check-equal-expr?
                 (go/expand-syntax (func (((slice t)))))
                 (go:func (cons #f #f) #f
                          (list (go:type:id
                                 'slice
                                 (go:type:id:slice (go:type:id 't 't))))
                          null null))
                (check-equal-expr?
                 (go/expand-syntax (func (((type)))))
                 (go:func (cons #f #f)
                          #f
                          (list (go:type:id 'type 'type))
                          null null))
                (check-equal-expr?
                 (go/expand-syntax (func (((ptr type)))))
                 (go:func (cons #f #f)
                          #f
                          (list (go:type:id 'ptr (go:type:id:ptr (go:type:id 'type 'type))))
                          null null))
                (check-equal-expr?
                 (go/expand-syntax (func (() ((returnType)))))
                 (go:func (cons #f #f)
                          #f null
                          (list (go:type:id 'returnType 'returnType))
                          null))
                (check-equal-expr?
                 (go/expand-syntax (func (() ((ptr returnType)))))
                 (go:func (cons #f #f)
                          #f null
                          (list (go:type:id 'ptr (go:type:id:ptr (go:type:id 'returnType 'returnType))))
                          null))
                (check-equal-expr?
                 (go/expand-syntax (func (((name type))
                                          ((returnName returnType)))))
                 (go:func (cons #f #f)
                          #f
                          `((name        . ,(go:type:id 'type 'type)))
                          `((returnName  . ,(go:type:id 'returnType 'returnType)))
                          null))

                (check-equal-expr?
                 (go/expand-syntax (func ((&rest t))))
                 (go:func (cons #f #f) #f
                          (list (go:func:type:variadic (go:type:id 't 't)))
                          null null))
                (check-equal-expr?
                 (go/expand-syntax (func (((&rest name type)))))
                 (go:func (cons #f #f)
                          #f
                          `((name . ,(go:func:type:variadic (go:type:id 'type 'type))))
                          null null))

                (check-equal-expr?
                 (go/expand-syntax (func (((name type) (name1 type1))
                                          ((returnName  returnType)
                                           (returnName1 returnType1)))))
                 (go:func (cons #f #f)
                          #f
                          `((name         . ,(go:type:id 'type 'type))
                            (name1        . ,(go:type:id 'type1 'type1)))
                          `((returnName   . ,(go:type:id 'returnType 'returnType))
                            (returnName1  . ,(go:type:id 'returnType1 'returnType1)))
                          null))
                (check-equal-expr?
                 (go/expand-syntax (func (((name type))
                                          ((returnName returnType)))
                                         (func (((name1 type1))
                                                ((returnName1 returnType1))))))
                 (go:func (cons #f #f)
                          #f
                          `((name       . ,(go:type:id 'type 'type)))
                          `((returnName . ,(go:type:id 'returnType 'returnType)))
                          (list (go:expr
                                 (go:func (cons #f #f)
                                          #f
                                          `((name1        . ,(go:type:id 'type1 'type1)))
                                          `((returnName1  . ,(go:type:id 'returnType1 'returnType1)))
                                          null)))))
                (check-equal-expr?
                 (go/expand-syntax (func (((name type))
                                          ((returnName returnType)))
                                         (func (((name1 type1))
                                                ((returnName1 returnType1))))
                                         (func (((name1 type1))
                                                ((returnName1 returnType1))))))
                 (go:func (cons #f #f)
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
                                          null))))))

    (test-suite "macro"
                (check-equal-expr?
                 (go/expand-syntax (macro (test) (quote (+ 1 1)))
                                   (test))
                 (go:macro 'test null (list (quote (quote (+ 1 1)))))
                 (go:func:call 'test null))
                (check-equal-expr?
                 (go/expand-syntax (macro (test (a b c)) (quote (+ a b c)))
                                   (test))
                 (go:macro 'test '(a b c) (list (quote (quote (+ a b c)))))
                 (go:func:call 'test null)))

    (test-suite "quote"
                (check-equal-expr?
                 (go/expand-syntax (quote (+ 1 1)))
                 (go:quote (quote (+ 1 1)))))

    (test-suite "var"
                (check-equal-expr?
                 (go/expand-syntax (var (x y)))
                 (go:var (list (go:var:binding 'x (go:type:id 'y 'y) #f))))
                (check-equal-expr?
                 (go/expand-syntax (var (x y 1)))
                 (go:var (list (go:var:binding 'x (go:type:id 'y 'y) (go:expr 1)))))
                (check-equal-expr?
                 (go/expand-syntax (var ((x y) 1)))
                 (go:var (list (go:var:binding 'x (go:type:id 'y 'y) (go:expr 1)))))
                (check-equal-expr?
                 (go/expand-syntax (var ((x) 1)))
                 (go:var (list (go:var:binding 'x #f (go:expr 1)))))
                (check-equal-expr?
                 (go/expand-syntax (var (x y 1) (xx yy 2)))
                 (go:var (list (go:var:binding 'x  (go:type:id 'y 'y)  (go:expr 1))
                               (go:var:binding 'xx (go:type:id 'yy 'yy) (go:expr 2))))))

    (test-suite "const"
                (check-equal-expr?
                 (go/expand-syntax (const (x y)))
                 (go:const (list (go:var:binding 'x (go:type:id 'y 'y) #f))))
                (check-equal-expr?
                 (go/expand-syntax (const (x y 1)))
                 (go:const (list (go:var:binding 'x (go:type:id 'y 'y) (go:expr 1)))))
                (check-equal-expr?
                 (go/expand-syntax (const (x y 1) (xx yy 2)))
                 (go:const (list (go:var:binding 'x  (go:type:id 'y 'y)  (go:expr 1))
                                 (go:var:binding 'xx (go:type:id 'yy 'yy) (go:expr 2))))))

    (test-suite "go"
                (check-equal-expr?
                 (go/expand-syntax (go (func)))
                 (go:go (go:expr (go:func (cons #f #f) #f null null null))))
                (check-equal-expr?
                 (go/expand-syntax (go ((func))))
                 (go:go (go:expr (go:func:call (go:func (cons #f #f) #f null null null)
                                               null)))))

    (test-suite "if"
                (check-equal-expr?
                 (go/expand-syntax (if (== 1 1) (fmt.Println "ok")))
                 (go:if
                  (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                  (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                  #f))
                (check-equal-expr?
                 (go/expand-syntax (if (== 1 1) (fmt.Println "ok") (fmt.Println "not ok")))
                 (go:if
                  (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                  (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                  (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))
                (check-equal-expr?
                 (go/expand-syntax (if (not (== 1 1)) (fmt.Println "ok") (fmt.Println "not ok")))
                 (go:if
                  (go:expr (go:operator '!
                                        (list (go:expr
                                               (go:operator '==
                                                            (list (go:expr 1) (go:expr 1)))))))
                  (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                  (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))
                (check-equal-expr?
                 (go/expand-syntax (if (not (== (+ 1 5) 1))
                                       (fmt.Println "ok")
                                       (fmt.Println "not ok")))
                 (go:if
                  (go:expr (go:operator '!
                                        (list (go:expr
                                               (go:operator
                                                '==
                                                (list (go:expr
                                                       (go:operator '+ (list (go:expr 1) (go:expr 5))))
                                                      (go:expr 1)))))))
                  (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                  (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))
                (check-equal-expr?
                 (go/expand-syntax (if (== 1 1) (begin (fmt.Println "ok"))))
                 (go:if
                  (go:expr (go:operator '== (list (go:expr 1) (go:expr 1))))
                  (go:expr
                   (go:begin (list (go:expr (go:func:call 'fmt.Println (list (go:expr "ok")))))))
                  #f)))

    (test-suite "when"
                (check-equal-expr?
                 (go/expand-syntax (when #t
                                     (fmt.Println 1)
                                     (fmt.Println 2)))
                 (go:if
                  (go:expr #t)
                  (go:begin
                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr 1))))
                         (go:expr (go:func:call 'fmt.Println (list (go:expr 2))))))
                  #f)))

    (test-suite "unless"
                (check-equal-expr?
                 (go/expand-syntax (unless #t
                                     (fmt.Println 1)
                                     (fmt.Println 2)))
                 (go:if
                  (go:operator '! (go:expr #t))
                  (go:begin
                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr 1))))
                         (go:expr (go:func:call 'fmt.Println (list (go:expr 2))))))
                  #f)))

    (test-suite "alias"
                (check-equal-expr?
                 (go/expand-syntax (alias errors New Errorf))
                 (go:alias "errors" '("New" "Errorf")))
                (check-equal-expr?
                 (go/expand-syntax (alias (errors New Errorf)
                                          (fmt Printf)))
                 (list (go:alias "errors" '("New" "Errorf"))
                       (go:alias "fmt"    '("Printf"))))
                (check-equal-expr?
                 (go/expand-syntax (alias errors New (rename (e Errorf))))
                 (go:alias "errors" '("New" ("e" . "Errorf"))))
                (check-equal-expr?
                 (go/expand-syntax (alias errors (prefix New (rename (Failure Error)))))
                 (go:alias "errors" '(("NewFailure" . "Error"))))
                (check-equal?
                 (go/expand-syntax (alias errors (suffix Example (rename (Failure Error)))))
                 (go/expand-syntax (alias errors (rename (FailureExample Error)))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx Foo Bar (prefix yyy (rename (Baz Qux)))))
                 (go:alias "xxx" '("Foo" "Bar" ("yyyBaz" . "Qux"))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx Foo Bar (suffix yyy Baz Qux)))
                 (go:alias "xxx" '("Foo" "Bar" ("Bazyyy" . "Baz") ("Quxyyy" . "Qux"))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx (const Foo Bar)))
                 (go:alias "xxx" (list
                                  (go:alias:const "Foo")
                                  (go:alias:const "Bar"))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx (type Foo Bar)))
                 (go:alias "xxx" (list
                                  (go:alias:type "Foo")
                                  (go:alias:type "Bar"))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx (prefix x y z) (type Foo Bar)))
                 (go:alias "xxx" (list '("xy" . "y")
                                       '("xz" . "z")
                                       (go:alias:type "Foo")
                                       (go:alias:type "Bar"))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx
                                          (prefix x y (rename (zz z)))
                                          (type Foo Bar)))
                 (go:alias "xxx" (list '("xy" . "y")
                                       '("xzz" . "z")
                                       (go:alias:type "Foo")
                                       (go:alias:type "Bar"))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx (type Bar (rename (foo Foo)))))
                 (go:alias "xxx" (list
                                  (go:alias:type "Bar")
                                  (go:alias:type '("foo" . "Foo")))))
                (check-equal-expr?
                 (go/expand-syntax (alias xxx (type (prefix x foo Foo) (prefix y Bar))))
                 (go:alias "xxx" (list
                                  (go:alias:type '("xfoo" . "foo"))
                                  (go:alias:type '("xFoo" . "Foo"))
                                  (go:alias:type '("yBar" . "Bar"))))))

    (test-suite "for"
                (check-equal-expr?
                 (go/expand-syntax (for () (fmt.Println k v)))
                 (go:for
                  null null #f #f #f
                  (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                (check-equal-expr?
                 (go/expand-syntax (for ((k v) (create (slice int) (1 2 3))) (fmt.Println k v)))
                 (go:for
                  (list 'k 'v)
                  (list (go:expr
                         (go:create
                          (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                          (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                  #f #f #f
                  (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                (check-equal-expr?
                 (go/expand-syntax (for ((k v) (range (create (slice int) (1 2 3)))) (fmt.Println k v)))
                 (go:for
                  (list 'k 'v)
                  (list (go:expr
                         (go:create
                          (go:type:id 'slice (go:type:id:slice (go:type:id 'int 'int)))
                          (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                  #f #f 'range
                  (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))
                (check-equal-expr?
                 (go/expand-syntax (for (k 10 (> k 0) (-- k)) (fmt.Println k)))
                 (go:for
                  (list 'k)
                  (list (go:expr 10))
                  (go:expr (go:operator '> (list (go:expr 'k) (go:expr 0))))
                  (go:expr (go:dec 'k))
                  #f
                  (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k))))))))

    (test-suite "begin"
                (check-equal-expr?
                 (go/expand-syntax (begin (fmt.Println k v) (+ 1 2)))
                 (go:begin
                  (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v))))
                        (go:expr (go:operator '+ (list (go:expr 1) (go:expr 2))))))))

    (test-suite "switch"
                (check-equal-expr?
                 (go/expand-syntax (switch 1
                                           (1 (fmt.Println "one"))
                                           (2 (fmt.Println "two"))
                                           (default (fmt.Println "default"))))
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
                (check-equal-expr?
                 (go/expand-syntax (switch (+ 1 1)
                                           (1 (fmt.Println "one"))
                                           (2 (fmt.Println "two"))
                                           (default (fmt.Println "default"))))
                 (go:switch (go:expr (go:operator '+ (list (go:expr 1) (go:expr 1))))
                            (list (go:case
                                   (go:expr 1)
                                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                  (go:case
                                   (go:expr 2)
                                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                  (go:case
                                   'default
                                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default"))))))))))

    (test-suite "cond"
                (check-equal-expr?
                 (go/expand-syntax (cond (1 (fmt.Println "one"))
                                         (2 (fmt.Println "two"))
                                         (default (fmt.Println "default"))))
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
                (check-equal-expr?
                 (go/expand-syntax (cond
                                     (1 (fmt.Println "one"))
                                     (2 (fmt.Println "two"))
                                     (default (fmt.Println "default"))))
                 (go:switch null
                            (list (go:case
                                   (go:expr 1)
                                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                  (go:case
                                   (go:expr 2)
                                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                  (go:case
                                   'default
                                   (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default"))))))))))

    (test-suite "select"
                (check-equal-expr?
                 (go/expand-syntax (select))
                 (go:select null))
                (check-equal-expr?
                 (go/expand-syntax (select (default (println "default case"))))
                 (go:select
                  (list
                   (go:case
                    'default
                    (list (go:expr (go:func:call 'println (list (go:expr "default case")))))))))
                (check-equal-expr?
                 (go/expand-syntax (select
                                    ((def x (<- ch)) (fmt.Printf "x: %+v\n" x))
                                    (default         (fmt.Println "default case"))))
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
                                 (list (go:expr "default case"))))))))))
    (test-suite "cast"
                (check-equal-expr? (go/expand-syntax (cast v bool))
                                   (go:cast (go:expr 'v) (go:type:id 'bool 'bool)))
                (check-equal-expr? (go/expand-syntax (cast v (assert bool)))
                                   (go:cast (go:expr 'v) (go:cast:assert (go:type:id 'bool 'bool)))))
    (test-suite "return"
                (check-equal-expr? (go/expand-syntax (return))
                                   (go:return null))
                (check-equal-expr? (go/expand-syntax (return (+ 1 1)))
                                   (go:return
                                    (list (go:expr
                                           (go:operator '+ (list
                                                            (go:expr 1)
                                                            (go:expr 1)))))))
                (check-equal-expr? (go/expand-syntax (return 1 1))
                                   (go:return (list (go:expr 1) (go:expr 1)))))
    (test-suite "break"
                (check-equal-expr? (go/expand-syntax (break))
                                   (go:break #f))
                (check-equal-expr? (go/expand-syntax (break xxx))
                                   (go:break 'xxx)))
    (test-suite "continue"
                (check-equal-expr? (go/expand-syntax (continue))
                                   (go:continue #f))
                (check-equal-expr? (go/expand-syntax (continue xxx))
                                   (go:continue 'xxx)))
    (test-suite "spread"
                (check-equal-expr? (go/expand-syntax (spread X))
                                   (go:spread (go:expr 'X))))
    (test-suite "label"
                (check-equal-expr?
                 (go/expand-syntax (label xxx (for () (break xxx))))
                 (go:label 'xxx
                           (go:expr (go:for null null #f #f #f
                                            (list (go:expr (go:break 'xxx))))))))
    (test-suite "goto"
                (check-equal-expr?
                 (go/expand-syntax (goto xxx))
                 (go:goto 'xxx)))
    (test-suite "iota"
                (check-equal-expr?
                 (go/expand-syntax (iota))
                 (go:iota)))
    (test-suite "defer"
                (check-equal-expr?
                 (go/expand-syntax (defer (println "hello")))
                 (go:defer (go:expr (go:func:call 'println
                                                  (list (go:expr "hello")))))))
    (test-suite "slice"
                (check-equal-expr?
                 (go/expand-syntax (slice arr 5 10))
                 (go:slice (go:expr 'arr)
                           (go:expr 5)
                           (go:expr 10)))
                (check-equal-expr?
                 (go/expand-syntax (slice arr 5))
                 (go:slice (go:expr 'arr)
                           (go:expr 5)
                           #f))
                (check-equal-expr?
                 (go/expand-syntax (slice arr 0 10))
                 (go:slice (go:expr 'arr)
                           (go:expr 0)
                           (go:expr 10)))
                (check-equal-expr?
                 (go/expand-syntax (slice arr (+ 1 1) 10))
                 (go:slice (go:expr 'arr)
                           (go:expr (go:operator '+ (list (go:expr 1) (go:expr 1))))
                           (go:expr 10))))

    (test-suite "index"
                (check-equal-expr?
                 (go/expand-syntax (index arr 0))
                 (go:index (go:expr 'arr) (go:expr 0)))
                (check-equal-expr?
                 (go/expand-syntax (index arr (* 2 x)))
                 (go:index (go:expr 'arr)
                           (go:expr (go:operator '*
                                                 (list (go:expr 2) (go:expr 'x)))))))

    (test-suite "key"
                (check-equal-expr?
                 (go/expand-syntax (key st Foo))
                 (go:key (go:expr 'st) '(Foo))))

    (test-suite "send"
                (check-equal-expr? (go/expand-syntax (send ch "test"))
                                   (go:send (go:expr 'ch)
                                            (go:expr "test")))
                (check-equal-expr? (go/expand-syntax (-> ch "test"))
                                   (go:send (go:expr 'ch)
                                            (go:expr "test"))))
    (test-suite "receive"
                (check-equal-expr? (go/expand-syntax (receive ch))
                                   (go:receive (go:expr 'ch)))
                (check-equal-expr? (go/expand-syntax (<- ch))
                                   (go:receive (go:expr 'ch))))
    (test-suite "inc"
                (check-equal-expr? (go/expand-syntax (inc n))
                                   (go:inc 'n))
                (check-equal-expr? (go/expand-syntax (++ n))
                                   (go:inc 'n)))
    (test-suite "dec"
                (check-equal-expr? (go/expand-syntax (dec n))
                                   (go:dec 'n))
                (check-equal-expr? (go/expand-syntax (-- n))
                                   (go:dec 'n)))
    (test-suite "ref"
                (check-equal-expr? (go/expand-syntax (ref v))
                                   (go:ref (go:expr 'v))))
    (test-suite "deref"
                (check-equal-expr? (go/expand-syntax (deref v))
                                   (go:deref (go:expr 'v))))

    ;; other expression cases

    (test-suite "primitive"
                (test-case "nil"
                  (check-equal-expr? (go/expand-syntax nil) 'nil))
                (test-case "bool"
                  (check-equal-expr? (go/expand-syntax #t) #t)
                  (check-equal-expr? (go/expand-syntax #f) #f))
                (test-case "number"
                  (check-equal-expr? (go/expand-syntax 666) 666)
                  (check-equal-expr? (go/expand-syntax 666.6) 666.6)
                  (check-equal-expr? (go/expand-syntax -666) -666)
                  (check-equal-expr? (go/expand-syntax -666.6) -666.6))
                (test-case "string"
                  (check-equal-expr? (go/expand-syntax "hello") "hello"))
                (test-case "identifier"
                  (check-equal-expr? (go/expand-syntax runtime.GOMAXPROCS) 'runtime.GOMAXPROCS)))

    (test-suite "dummy"
                (check-equal-expr? (go/expand-syntax (package main)
                                                     (import os fmt)
                                                     (func (main) (println os.Args)))
                                   (go:package 'main)
                                   (go:imports (list (go:import 'os #f) (go:import 'fmt #f)))
                                   (go:func (cons #f #f) 'main null null
                                            (list (go:expr (go:func:call 'println
                                                                         (list (go:expr 'os.Args))))))))

    (test-suite "complex"
                (test-case "cli"
                  (check-equal-expr?
                   (go/expand-syntax (package main)
                                     (import
                                      os fmt
                                      (cli github.com/urfave/cli/v2))

                                     (var (Flags (slice cli.Flag)
                                                 (create (slice cli.Flag)
                                                         ((create cli.BoolFlag
                                                                  ((Name "test")
                                                                   (Usage "test flag")))))))

                                     (func (RootAction ((ctx (ptr cli.Context))) (error))
                                           (fmt.Println "hello from root, test is" (ctx.Bool "test")))

                                     (func (main)
                                           (def app (create (ptr cli.App)))
                                           (set app.Flags    Flags)
                                           (set app.Action   RootAction)
                                           (app.Run os.Args)))

                   (go:package 'main)
                   (go:imports (list (go:import 'os #f)
                                     (go:import 'fmt #f)
                                     (go:import 'github.com/urfave/cli/v2 'cli)))
                   (go:var (list (go:var:binding
                                  'Flags
                                  (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag 'cli.Flag)))
                                  (go:expr (go:create
                                            (go:type:id 'slice (go:type:id:slice (go:type:id 'cli.Flag 'cli.Flag)))
                                            (list (go:expr (go:create
                                                            (go:type:id 'cli.BoolFlag 'cli.BoolFlag)
                                                            (list (cons 'Name  (go:expr "test"))
                                                                  (cons 'Usage (go:expr "test flag")))))))))))
                   (go:func (cons #f #f)
                            'RootAction
                            (list (cons 'ctx (go:type:id 'ptr (go:type:id:ptr (go:type:id 'cli.Context 'cli.Context)))))
                            (list (go:type:id 'error 'error))
                            (list (go:expr
                                   (go:func:call 'fmt.Println
                                                 (list (go:expr "hello from root, test is")
                                                       (go:expr (go:func:call 'ctx.Bool
                                                                              (list (go:expr "test")))))))))
                   (go:func (cons #f #f)
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
                                                         (list (go:expr 'os.Args))))))))))))
