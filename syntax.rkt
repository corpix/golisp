#lang racket/base
(require (except-in racket/list flatten)
         racket/bool
         racket/match
         racket/string
         racket/format
         racket/set
         racket/syntax
	 syntax/parse
         "macro.rkt"
         "tool.rkt"
         (for-syntax (except-in racket/list flatten)
                     racket/base
                     racket/match
                     racket/format
                     racket/syntax
                     syntax/parse
                     "type.rkt"
                     "macro.rkt"
                     "tool.rkt"))

(provide (all-defined-out)
         (for-syntax (all-from-out "macro.rkt")
                     (all-defined-out)))

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

(define-syntax (define-gosyntax-class-map stx)
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
               (syntax (define-gosyntax (id s)
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
       bitwise-and ;; | is a reader in racket, so & | are problematic
       bitwise-or
       ^  bitwise-xor
       << bitwise-left-shift
       >> bitwise-right-shift)
    (pattern ((~or* op:+ op:-) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:% op:* op:/) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* op:== op:!= op:> op:< op:>= op:<=) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax op))
                                     (attribute xs.ast)))
    (pattern ((~or* ! not) x:ExprRecur)
             #:attr ast (go:operator (*->symbol (syntax !))
                                     (list (attribute x.ast))))
    (pattern ((~or* && and) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax &&))
                                     (attribute xs.ast)))
    (pattern ((~or* \|\| or) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax \|\|))
                                     (attribute xs.ast)))
    (pattern (bitwise-and xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax &))
                                     (attribute xs.ast)))
    (pattern (bitwise-or xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax \|))
                                     (attribute xs.ast)))
    (pattern ((~or* ^ bitwise-xor) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax ^))
                                     (attribute xs.ast)))
    (pattern ((~or* << bitwise-left-shift) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax <<))
                                     (attribute xs.ast)))
    (pattern ((~or* >> bitwise-right-shift) xs:ExprRecur ...+)
             #:attr ast (go:operator (*->symbol (syntax >>))
                                     (attribute xs.ast))))

  ;;

  (define-syntax-class TypeIdMap
    #:description "map type description"
    #:attributes (id kind ast)
    #:datum-literals (map)
    (pattern (map k:TypeId v:TypeId)
             #:attr id   (quote map)
             #:attr kind (quote complex)
             #:attr ast  (go:type:id:map (attribute k.ast)
                                         (attribute v.ast))))

  (define-syntax-class TypeIdStruct
    #:description "struct type description"
    #:attributes (id kind ast)
    #:datum-literals (struct)
    (pattern (struct xs:TypeIdStruct/Field ...)
             #:attr id   (quote struct)
             #:attr kind (quote complex)
             #:attr ast  (go:type:id:struct (attribute xs.ast))))

  (define-syntax-class TypeIdStruct/Field
    #:description "struct type field"
    #:attributes (ast)
    (pattern (k:id v:TypeId (~optional tag:string #:defaults ((tag (syntax #f)))))
             #:attr ast (go:type:id:struct:field
                         (*->symbol (syntax k))
                         (attribute v.ast)
                         (syntax->datum (syntax tag))))
    (pattern v:TypeId
             #:attr ast (go:type:id:struct:field #f (attribute v.ast) #f)))

  (define-syntax-class TypeIdInterface
    #:description "interface type description"
    #:attributes (id kind ast)
    #:datum-literals (interface)
    (pattern (interface xs:TypeIdInterface/Field ...)
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
    (pattern (slice t:TypeId)
             #:attr id   (quote slice)
             #:attr kind (quote complex)
             #:attr ast  (go:type:id:slice (attribute t.ast))))

  (define-syntax-class TypeIdArray
    #:description "array type description"
    #:attributes (id kind ast)
    #:datum-literals (array ...)
    (pattern (array t:TypeId (~or* size:integer size:...))
             #:attr id (quote array)
             #:attr kind (quote complex)
             #:attr ast (go:type:id:array (attribute t.ast) (syntax->datum (syntax size)))))

  (define-syntax-class TypeIdPtr
    #:description "pointer type description"
    #:attributes (id kind ast)
    #:datum-literals (ptr)
    (pattern (ptr t:TypeId)
             #:attr id   (quote ptr)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:ptr (attribute t.ast))))

  (define-syntax-class TypeIdChan
    #:description "chan type description"
    #:attributes (id kind ast)
    #:datum-literals (chan -> <-)
    (pattern (chan (~optional (~or* direction:-> direction:<-)
                              #:defaults ((direction (syntax #f))))
                   t:TypeId)
             #:attr id   (quote chan)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:chan (syntax->datum (syntax direction)) (attribute t.ast))))

  (define-syntax-class TypeIdFunc
    #:description "func type description"
    #:attributes (id kind ast)
    #:datum-literals (func)
    (pattern (func (~optional i:FuncIO #:defaults ((i (syntax null))))
                   (~optional o:FuncIO #:defaults ((o (syntax null)))))
             #:attr id   (quote func)
             #:attr kind (quote primitive)
             #:attr ast  (go:type:id:func
                          (or (attribute i.ast) null)
                          (or (attribute o.ast) null))))

  (define-syntax-class TypeId%
    #:description "custom user type description"
    #:attributes (id kind ast)
    (pattern t:id
             #:attr id   (syntax->datum (syntax t))
             #:attr kind (quote complex)
             #:attr ast  #f))

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
    (pattern (cast v:TypeId)
             #:attr kind (attribute v.kind)
             #:attr ast  (attribute v.ast))
    (pattern (ref v:TypeId)
             #:attr kind (attribute v.kind)
             #:attr ast  (go:ref   (attribute v.ast)))
    (pattern (deref v:TypeId)
             #:attr kind (attribute v.kind)
             #:attr ast  (go:deref (attribute v.ast))))

  (define-syntax-class Type
    #:description "type definition"
    #:attributes (kind ast)
    #:datum-literals (type)
    (pattern (type t:TypeId)
             #:attr kind (attribute t.kind)
             #:attr ast  (go:type #f (attribute t.ast)))
    (pattern (type (name:id t:TypeId))
             #:attr kind (attribute t.kind)
             #:attr ast  (go:type (attribute name) (attribute t.ast))))

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
    #:datum-literals (create slice array struct map )
    (pattern (create t:TypeId)
             #:attr ast (go:create (attribute t.ast) null))
    (pattern (create t:TypeId (xs:CompositeTypeInitializer ...))
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
    (pattern (def (k:Expr ...) (v:ExprRecur ...))
             #:attr ast (go:def (attribute k.ast)
                                (attribute v.ast))))

  (define-syntax-class Set
    #:description "variable initialization"
    #:attributes (ast)
    #:datum-literals (set)
    (pattern (set k:Expr v:ExprRecur)
             #:attr ast (go:set (list (attribute k.ast))
                                (list (attribute v.ast))))
    (pattern (set (k:Expr ...) (v:ExprRecur ...))
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
    (pattern (type:TypeId ...)
             #:attr ast (attribute type.ast))
    (pattern ((name:id type:TypeId) ...)
             #:attr ast (map (lambda (n t) (cons n t))
                             (syntax->list (syntax (name ...)))
                             (attribute type.ast))))

  (define-syntax-class Func
    #:description "named function definition or lambda expression"
    #:attributes (ast)
    #:datum-literals (func struct)
    (pattern (func () body:ExprRecur ...)
             #:attr ast (go:func (cons #f #f) #f null null
                                 (attribute body.ast)))
    (pattern (func ((~optional i:FuncIO #:defaults ((i (syntax null))))
                    (~optional o:FuncIO #:defaults ((o (syntax null)))))
                   body:ExprRecur ...)
             #:attr ast (go:func (cons #f #f) #f
                                 (or (attribute i.ast) null)
                                 (or (attribute o.ast) null)
                                 (attribute body.ast)))
    (pattern (func ((~or* name:id (name:id (struct-binding:id struct-type:TypeId)))
                    (~optional i:FuncIO #:defaults ((i (syntax null))))
                    (~optional o:FuncIO #:defaults ((o (syntax null)))))
                   body:ExprRecur ...)
             #:attr ast (go:func (cons (attribute struct-type.ast)
                                       (attribute struct-binding))
                                 (and (syntax->datum (syntax name)) (*->symbol (syntax name)))
                                 (or (attribute i.ast) null)
                                 (or (attribute o.ast) null)
                                 (attribute body.ast))))

  (define-syntax-class FuncCall
    #:description "function call"
    #:attributes (ast)
    (pattern (r:id xs:ExprRecur ...)
             #:attr ast (go:func:call (syntax->datum (syntax r))
                                      (attribute xs.ast)))
    (pattern (r:Func xs:ExprRecur ...)
             #:attr ast (go:func:call (attribute r.ast)
                                      (attribute xs.ast)))
    (pattern (r:ExprRecur xs:ExprRecur ...)
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
    (pattern (name:id type:TypeId value:ExprRecur)
             #:attr ast (go:var:binding (syntax->datum (syntax name))
                                        (attribute type.ast)
                                        (attribute value.ast))))

  (define-syntax-class Var
    #:description "variable definition"
    #:attributes (ast)
    #:datum-literals (var const)
    (pattern (var binding:VarBinding ...+)
             #:attr ast (go:var (attribute binding.ast)))
    (pattern (const binding:VarBinding ...+)
             #:attr ast (go:const (attribute binding.ast))))

  ;;

  (define-syntax-class Go
    #:description "go routine invocation"
    #:attributes (ast)
    #:datum-literals (go)
    (pattern (go expr:ExprRecur) #:attr ast (go:go (attribute expr.ast))))

  ;;

  (define-syntax-class If
    #:description "if statement"
    #:attributes (ast)
    #:datum-literals (if)
    (pattern (if condition:ExprRecur then:ExprRecur
                 (~optional else:ExprRecur #:defaults ((else (syntax #f)))))
             #:attr ast (go:if (attribute condition.ast)
                               (attribute then.ast)
                               (attribute else.ast))))

  (define-syntax-class When
    #:description "when statement"
    #:attributes (ast)
    #:datum-literals (when)
    (pattern (when condition:ExprRecur body:ExprRecur ...)
             #:attr ast (go:if (attribute condition.ast)
                               (go:begin (attribute body.ast))
                               #f)))

  (define-syntax-class Unless
    #:description "unless statement"
    #:attributes (ast)
    #:datum-literals (unless)
    (pattern (unless condition:ExprRecur then:ExprRecur ...)
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
    (pattern (prefix ~! sym:AliasSym xs:AliasSyms ...)
             #:attr ast (map
                         (lambda (vv builder)
                           (builder (lambda (v) (cons (~a (car (attribute sym.ast)) v) v))
                            vv))
                         (attribute xs.ast)
                         (attribute xs.builder))
             #:attr builder alias-rename-builder)
    (pattern (suffix ~! sym:AliasSym xs:AliasSyms ...)
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
    (pattern (rename ~! (new-name:AliasSym old-name:AliasSym) ...)
             #:attr ast (map
                         (lambda (new old) (cons new old))
                         (flatten (attribute new-name.ast))
                         (flatten (attribute old-name.ast)))
             #:attr builder alias-rename-builder))

  (define-syntax-class AliasConst
    #:description "alias const expression (const xs ...)"
    #:attributes (ast builder)
    #:datum-literals (const)
    (pattern (const ~! (~or* xs:AliasWrap xs:AliasRename xs:AliasSyms) ...)
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
    (pattern (type ~! (~or* xs:AliasWrap xs:AliasRename xs:AliasSyms) ...)
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
    (pattern (alias ~! (~or* ns:id ns:string) xs:AliasSyms ...+)
             #:attr ast (go:alias
                         (*->string (syntax->datum (attribute ns)))
                         (flatten (attribute xs.ast)))))

  ;;

  (define-syntax-class For
    #:description "for statement"
    #:attributes (ast)
    #:datum-literals (for range)
    (pattern (for ((~optional vars:ForVars   #:defaults ((vars (syntax #f))))
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
    (pattern (vars:id ...+) #:attr ast (syntax->list (syntax (vars ...))))
    (pattern vars:id        #:attr ast (list (syntax->datum (syntax vars)))))

  (define-splicing-syntax-class ForSeq
    #:description "for statement sequence"
    #:attributes (kind ast)
    #:datum-literals (range)
    (pattern (k:range seq:ExprRecur)
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
    (pattern (begin exprs:ExprRecur ...+)
             #:attr ast (go:begin (attribute exprs.ast))))

  ;;

  (define-syntax-class Switch
    #:description "switch statement"
    #:attributes (ast)
    #:datum-literals (switch)
    (pattern (switch value:ExprRecur cases:Case ...+)
             #:attr ast (go:switch (attribute value.ast)
                                   (attribute cases.ast))))

  (define-syntax-class Cond
    #:description "cond statement"
    #:attributes (ast)
    #:datum-literals (cond)
    (pattern (cond cases:Case ...+)
             #:attr ast (go:switch null (attribute cases.ast))))

  ;;

  (define-syntax-class Select
    #:description "select statement"
    #:attributes (ast)
    #:datum-literals (select)
    (pattern (select cases:Case ...+)
             #:attr ast (go:select (attribute cases.ast))))

  ;;

  (define-syntax-class Case
    #:description "switch statement case"
    #:attributes (ast)
    #:datum-literals (default)
    (pattern (default body:ExprRecur ...)
             #:attr ast (go:case 'default
                                 (attribute body.ast)))
    (pattern (pred:ExprRecur body:ExprRecur ...)
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
    (pattern (return values:ExprRecur ...)
             #:attr ast (go:return (attribute values.ast))))

  (define-syntax-class Break
    #:description "break statement"
    #:attributes (ast)
    #:datum-literals (break)
    (pattern (break (~optional label:id #:defaults ((label (syntax #f)))))
             #:attr ast (go:break (syntax-e (syntax label)))))

  (define-syntax-class Continue
    #:description "continue statement"
    #:attributes (ast)
    #:datum-literals (continue)
    (pattern (continue (~optional label:id #:defaults ((label (syntax #f)))))
             #:attr ast (go:continue (syntax-e (syntax label)))))

  (define-syntax-class Spread
    #:description "spread (value...) statement"
    #:attributes (ast)
    #:datum-literals (spread)
    (pattern (spread expr:ExprRecur) #:attr ast (go:spread (attribute expr.ast))))

  ;;

  (define-syntax-class Label
    #:description "labeled statement"
    #:attributes (ast)
    #:datum-literals (label)
    (pattern (label name:id e:ExprRecur)
             #:attr ast (go:label (syntax-e (syntax name))
                                  (attribute e.ast))))

  (define-syntax-class Goto
    #:description "goto statement"
    #:attributes (ast)
    #:datum-literals (goto)
    (pattern (goto label:id)
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
    (pattern (defer body:ExprRecur) #:attr ast (go:defer (attribute body.ast))))

  ;;

  (define-syntax-class Slice
    #:description "slicing expression"
    #:attributes (ast)
    #:datum-literals (slice)
    (pattern (slice value:ExprRecur
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
    (pattern (index value:ExprRecur key:ExprRecur)
             #:attr ast (go:index (attribute value.ast)
                                  (attribute key.ast))))

  (define-syntax-class Key
    #:description "key expression"
    #:attributes (ast)
    #:datum-literals (key)
    (pattern (key object:ExprRecur k:id ...)
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
    (pattern ((~or* receive <-) chan:ExprRecur)
             #:attr ast (go:receive (attribute chan.ast))))

  ;;

  (define-syntax-class Inc
    #:description "increment identifier value"
    #:attributes (ast)
    #:datum-literals (inc ++)
    (pattern ((~or* inc ++) id:id)
             #:attr ast (go:inc (*->symbol (syntax id)))))

  (define-syntax-class Dec
    #:description "decrement identifier value"
    #:attributes (ast)
    #:datum-literals (dec --)
    (pattern ((~or* dec --) id:id)
             #:attr ast (go:dec (*->symbol (syntax id)))))

  ;;

  (define-syntax-class Ref
    #:description "reference (get a pointer) of the expression"
    #:attributes (ast)
    #:datum-literals (ref)
    (pattern (ref ex:ExprRecur)
             #:attr ast (go:ref (attribute ex.ast))))

  (define-syntax-class Deref
    #:description "dereference (get a pointer) of the expression"
    #:attributes (ast)
    #:datum-literals (deref)
    (pattern (deref ex:ExprRecur)
             #:attr ast (go:deref (attribute ex.ast))))

  ;;

  (define-syntax-class Expr
    #:description "expression"
    #:attributes (ast)
    #:datum-literals (nil)
    #:commit
    (pattern (~commit (~or* v:Operator
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
                            v:Func))
             #:attr ast (go:expr (attribute v.ast)))
    (pattern (~or* v:id v:boolean v:number v:string v:nil)
             #:attr ast (go:expr (syntax->datum (syntax v)))))

  (define-syntax-class ExprRecur
    #:description "recursive expression"
    #:attributes (ast)
    (pattern v:Expr #:attr ast (attribute v.ast))
    (pattern v:FuncCall ;; NOTE: FuncCall class should be the last pattern, it acts as a catch-all for sexp's
             #:attr ast (go:expr (attribute v.ast)))))

;;

(define-gosyntax-class-map
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
  (func     Func))

;;

(define-gosyntax (prog stx)
  (syntax-parse stx
    ((_ xs:ExprRecur ...) (attribute xs.ast))))

(define-gosyntax (prelude stx)
  (syntax-parse stx
    ((_ xs:ExprRecur ...)
     (begin0 (syntax (void))
       (set-box! (*prelude*)
                 (append (unbox (*prelude*))
                         (expand-macro (attribute xs.ast))))))))

(define-gosyntax (epilogue stx)
  (syntax-parse stx
    ((_ xs:ExprRecur ...)
     (begin0 (syntax (void))
       (set-box! (*epilogue*)
                 (append (unbox (*epilogue*))
                         (expand-macro (attribute xs.ast))))))))

(define-syntax (go/expand stx)
  (syntax-parse stx
    ((_ ex:ExprRecur ...+)
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

(module+ test
  (require rackunit
           rackunit/text-ui

           "type.rkt")

  (define-syntax (test-case/operator stx)
    (syntax-parse stx
      ((_ op:id e:expr r:expr)
       (syntax (test-case (symbol->string (quote operator))
                 (check-equal? (go/expand e)
                               (list (go:expr (go:operator (quote op) r)))))))))

  (for
      ((suite (list
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
                           (check-equal?
                            (go/expand (type X))
                            (list (go:expr (go:type #f (go:type:id 'X #f)))))
                           (check-equal?
                            (go/expand (type (map string string)))
                            (list (go:expr (go:type #f (go:type:id 'map (go:type:id:map (go:type:id 'string #f)
                                                                                        (go:type:id 'string #f)))))))
                           (check-equal?
                            (go/expand (type (map string (map int X))))
                            (list (go:expr
                                   (go:type #f (go:type:id 'map (go:type:id:map
                                                                 (go:type:id 'string #f)
                                                                 (go:type:id 'map
                                                                             (go:type:id:map
                                                                              (go:type:id 'int #f)
                                                                              (go:type:id 'X #f)))))))))
                           (check-equal?
                            (go/expand (type (struct io.Reader (x (map string string)) (y X))))
                            (list (go:expr
                                   (go:type #f (go:type:id 'struct (go:type:id:struct
                                                                    (list (go:type:id:struct:field #f (go:type:id 'io.Reader #f) #f)
                                                                          (go:type:id:struct:field 'x (go:type:id 'map (go:type:id:map
                                                                                                                        (go:type:id 'string #f)
                                                                                                                        (go:type:id 'string #f)))
                                                                                                   #f)
                                                                          (go:type:id:struct:field 'y (go:type:id 'X #f) #f))))))))
                           (check-equal?
                            (go/expand (type (interface io.Reader (x (func)))))
                            (list (go:expr (go:type #f (go:type:id 'interface
                                                                   (go:type:id:interface
                                                                    (list (go:type:id:interface:field #f (go:type:id 'io.Reader #f))
                                                                          (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))))))
                           (check-equal?
                            (go/expand (type (interface io.Reader (x (func () ())))))
                            (list (go:expr (go:type #f (go:type:id 'interface
                                                                   (go:type:id:interface
                                                                    (list (go:type:id:interface:field #f (go:type:id 'io.Reader #f))
                                                                          (go:type:id:interface:field 'x (go:type:id 'func (go:type:id:func null null))))))))))
                           (check-equal?
                            (go/expand
                             (type
                              (interface io.Reader
                                (x (func ((k int) (v (map int string))) (error)))
                                (y (struct
                                     (x (interface))
                                     (y (map bool (struct))))))))
                            (list (go:expr (go:type #f (go:type:id 'interface
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

                           (check-equal?
                            (go/expand (type (slice   string)))
                            (list (go:expr (go:type #f (go:type:id 'slice (go:type:id:slice    (go:type:id 'string #f)))))))
                           (check-equal?
                            (go/expand (type (array   string 5)))
                            (list (go:expr (go:type #f (go:type:id 'array (go:type:id:array    (go:type:id 'string #f) 5))))))
                           (check-equal?
                            (go/expand (type (ptr     string)))
                            (list (go:expr (go:type #f (go:type:id 'ptr   (go:type:id:ptr      (go:type:id 'string #f)))))))
                           (check-equal?
                            (go/expand (type (chan -> string)))
                            (list (go:expr (go:type #f (go:type:id 'chan  (go:type:id:chan '-> (go:type:id 'string #f)))))))
                           (check-equal?
                            (go/expand (type (chan <- string)))
                            (list (go:expr (go:type #f (go:type:id 'chan  (go:type:id:chan '<- (go:type:id 'string #f)))))))
                           (check-equal?
                            (go/expand (type (chan    string)))
                            (list (go:expr (go:type #f (go:type:id 'chan  (go:type:id:chan #f  (go:type:id 'string #f)))))))
                           (check-equal?
                            (go/expand (type (chan    (struct))))
                            (list (go:expr (go:type #f (go:type:id 'chan  (go:type:id:chan #f  (go:type:id 'struct (go:type:id:struct null))))))))

                           (check-equal?
                            (go/expand (type (func ((k string) (v int)) (int error))))
                            (list (go:expr
                                   (go:type
                                    #f
                                    (go:type:id
                                     'func
                                     (go:type:id:func
                                      (list (cons 'k (go:type:id 'string #f))
                                            (cons 'v (go:type:id 'int    #f)))
                                      (list (go:type:id 'int   #f)
                                            (go:type:id 'error #f))))))))
                           (check-equal?
                            (go/expand (type (func (string int) (int error))))
                            (list (go:expr
                                   (go:type
                                    #f
                                    (go:type:id
                                     'func
                                     (go:type:id:func
                                      (list (go:type:id 'string #f)
                                            (go:type:id 'int    #f))
                                      (list (go:type:id 'int   #f)
                                            (go:type:id 'error #f))))))))

                           (check-equal?
                            (go/expand (type (name (chan (struct)))))
                            (list (go:expr
                                   (go:type 'name
                                            (go:type:id 'chan
                                                        (go:type:id:chan #f
                                                                         (go:type:id 'struct (go:type:id:struct null)))))))))

               (test-suite "create"
                           (check-equal?
                            (go/expand (create X))
                            (list (go:expr (go:create (go:type:id 'X #f) null))))
                           (check-equal?
                            (go/expand (create X nil))
                            (list (go:expr (go:create (go:type:id 'X #f) (go:expr 'nil)))))
                           (check-equal?
                            (go/expand (create (cast X) nil))
                            (list (go:expr (go:create (go:type:id 'X #f) (go:expr 'nil)))))
                           (check-equal?
                            (go/expand (create (ref X) nil))
                            (list (go:expr (go:create (go:ref (go:type:id 'X #f))
                                                      (go:expr 'nil)))))
                           (check-equal?
                            (go/expand (create (deref X) nil))
                            (list (go:expr (go:create (go:deref (go:type:id 'X #f))
                                                      (go:expr 'nil)))))
                           (check-equal?
                            (go/expand (create (slice int) (1 2 3 4)))
                            (list (go:expr (go:create (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                      (list (go:expr 1) (go:expr 2)
                                                            (go:expr 3) (go:expr 4))))))
                           (check-equal?
                            (go/expand (create (array int 4) (1 2 3 4)))
                            (list (go:expr (go:create (go:type:id 'array (go:type:id:array (go:type:id 'int #f) 4))
                                                      (list (go:expr 1) (go:expr 2)
                                                            (go:expr 3) (go:expr 4))))))
                           (check-equal?
                            (go/expand (create (map string int)
                                               (("1" 1)
                                                ("2" 2)
                                                ("3" 3)
                                                ("4" 4))))
                            (list (go:expr (go:create (go:type:id 'map
                                                                  (go:type:id:map (go:type:id 'string #f)
                                                                                  (go:type:id 'int    #f)))
                                                      (list (cons "1" (go:expr 1)) (cons "2" (go:expr 2))
                                                            (cons "3" (go:expr 3)) (cons "4" (go:expr 4)))))))

                           (check-equal?
                            (go/expand (create (struct (x int)) (1)))
                            (list
                             (go:expr
                              (go:create (go:type:id 'struct
                                                     (go:type:id:struct
                                                      (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                         (list (go:expr 1))))))
                           (check-equal?
                            (go/expand (create (struct (x (interface))) ((NewReader))))
                            (list
                             (go:expr
                              (go:create (go:type:id 'struct
                                                     (go:type:id:struct
                                                      (list (go:type:id:struct:field 'x (go:type:id 'interface (go:type:id:interface null)) #f))))
                                         (list (go:expr (go:func:call 'NewReader null)))))))
                           (check-equal?
                            (go/expand (create (struct (x (interface)))))
                            (list (go:expr (go:create (go:type:id 'struct
                                                                  (go:type:id:struct (list (go:type:id:struct:field
                                                                                            'x
                                                                                            (go:type:id 'interface (go:type:id:interface null))
                                                                                            #f))))
                                                      null))))
                           (check-equal?
                            (go/expand (create (map string (struct (x int)))
                                               (("1" (create (struct (x int)) (x 1)))
                                                ("2" (create (struct (x int)) (x 2)))
                                                ("3" (create (struct (x int)) (x 3)))
                                                ("4" (create (struct (x int)) (x 4))))))
                            (list (go:expr
                                   (go:create
                                    (go:type:id 'map
                                                (go:type:id:map
                                                 (go:type:id 'string #f)
                                                 (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))))
                                    (list (cons "1" (go:expr (go:create
                                                              (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                              (list (go:expr 'x) (go:expr 1)))))
                                          (cons "2" (go:expr (go:create
                                                              (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                              (list (go:expr 'x) (go:expr 2)))))
                                          (cons "3" (go:expr (go:create
                                                              (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                              (list (go:expr 'x) (go:expr 3)))))
                                          (cons "4" (go:expr (go:create
                                                              (go:type:id 'struct (go:type:id:struct (list (go:type:id:struct:field 'x (go:type:id 'int #f) #f))))
                                                              (list (go:expr 'x) (go:expr 4)))))))))))

               (test-suite "def"
                           (check-equal?
                            (go/expand (def x 1))
                            (list (go:expr (go:def (list (go:expr 'x)) (list (go:expr 1))))))
                           (check-equal?
                            (go/expand (def (x y) (1 2)))
                            (list (go:expr (go:def (list (go:expr 'x) (go:expr 'y))
                                                   (list (go:expr 1) (go:expr 2))))))
                           (check-equal?
                            (go/expand (def x (func ())))
                            (list (go:expr (go:def (list (go:expr 'x))
                                                   (list (go:expr (go:func (cons #f #f) #f null null null)))))))
                           (check-equal?
                            (go/expand (def x (create (slice int))))
                            (list (go:expr (go:def (list (go:expr 'x))
                                                   (list (go:expr (go:create
                                                                   (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                                   null))))))))

               (test-suite "set"
                           (check-equal?
                            (go/expand (set x 1))
                            (list (go:expr (go:set (list (go:expr 'x))
                                                   (list (go:expr 1))))))
                           (check-equal?
                            (go/expand (set (x y) (1 2)))
                            (list (go:expr (go:set (list (go:expr 'x) (go:expr 'y))
                                                   (list (go:expr 1) (go:expr 2))))))
                           (check-equal?
                            (go/expand (set x (func ())))
                            (list (go:expr (go:set (list (go:expr 'x))
                                                   (list (go:expr (go:func (cons #f #f) #f null null null)))))))
                           (check-equal?
                            (go/expand (set x (create (slice int))))
                            (list (go:expr (go:set (list (go:expr 'x))
                                                   (list (go:expr (go:create
                                                                   (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                                   null))))))))

               (test-suite "package"
                           (check-equal?
                            (go/expand (package foo))
                            (list (go:expr (go:package 'foo))))
                           (check-equal?
                            (go/expand (package #:foo))
                            (list (go:expr (go:package 'foo))))
                           (check-equal?
                            (go/expand (package "foo"))
                            (list (go:expr (go:package 'foo)))))

               (test-suite "import"
                           (check-equal?
                            (go/expand (import foo bar))
                            (list (go:expr (go:imports
                                            (list
                                             (go:import 'foo #f)
                                             (go:import 'bar #f))))))
                           (check-equal?
                            (go/expand (import #:foo bar))
                            (list (go:expr (go:imports
                                            (list
                                             (go:import 'foo #f)
                                             (go:import 'bar #f))))))
                           (check-equal?
                            (go/expand (import (x #:foo) bar))
                            (list (go:expr (go:imports
                                            (list
                                             (go:import 'foo 'x)
                                             (go:import 'bar #f))))))
                           (check-equal?
                            (go/expand (import (#:x #:foo) bar))
                            (list (go:expr (go:imports
                                            (list
                                             (go:import 'foo 'x)
                                             (go:import 'bar #f))))))
                           (check-equal?
                            (go/expand (import (#:x "foo") "bar"))
                            (list (go:expr (go:imports
                                            (list
                                             (go:import 'foo 'x)
                                             (go:import 'bar #f)))))))

               (test-suite "func"
                           (check-equal?
                            (go/expand (func ()))
                            (list (go:expr (go:func (cons #f #f) #f null null null))))
                           (check-equal?
                            (go/expand (func (() ())))
                            (list (go:expr (go:func (cons #f #f) #f null null null))))
                           (check-equal?
                            (go/expand (func (hello () ())))
                            (list (go:expr (go:func (cons #f #f) 'hello null null null))))
                           (check-equal?
                            (go/expand (func ((hello (s (ptr Struct))) () ())))
                            (list (go:expr (go:func (cons (go:type:id 'ptr (go:type:id:ptr (go:type:id 'Struct #f))) 's)
                                                    'hello
                                                    null null null))))
                           (check-equal?
                            (go/expand (func ((t) ())))
                            (list (go:expr (go:func (cons #f #f) #f (list (go:type:id 't #f)) null null))))
                           (check-equal?
                            (go/expand (func (((slice t)) ())))
                            (list (go:expr (go:func (cons #f #f) #f
                                                    (list (go:type:id
                                                           'slice
                                                           (go:type:id:slice (go:type:id 't #f))))
                                                    null null))))
                           (check-equal?
                            (go/expand (func (((name type))
                                              ((returnName returnType)))))
                            (list (go:expr
                                   (go:func (cons #f #f)
                                            #f
                                            `((name        . ,(go:type:id 'type       #f)))
                                            `((returnName  . ,(go:type:id 'returnType #f)))
                                            null))))
                           (check-equal?
                            (go/expand (func (((name type) (name1 type1))
                                              ((returnName  returnType)
                                               (returnName1 returnType1)))))
                            (list (go:expr
                                   (go:func (cons #f #f)
                                            #f
                                            `((name         . ,(go:type:id 'type        #f))
                                              (name1        . ,(go:type:id 'type1       #f)))
                                            `((returnName   . ,(go:type:id 'returnType  #f))
                                              (returnName1  . ,(go:type:id 'returnType1 #f)))
                                            null))))
                           (check-equal?
                            (go/expand (func (((name type))
                                              ((returnName returnType)))
                                             (func (((name1 type1))
                                                    ((returnName1 returnType1))))))
                            (list (go:expr
                                   (go:func (cons #f #f)
                                            #f
                                            `((name       . ,(go:type:id 'type       #f)))
                                            `((returnName . ,(go:type:id 'returnType #f)))
                                            (list (go:expr
                                                   (go:func (cons #f #f)
                                                            #f
                                                            `((name1        . ,(go:type:id 'type1       #f)))
                                                            `((returnName1  . ,(go:type:id 'returnType1 #f)))
                                                            null)))))))
                           (check-equal?
                            (go/expand (func (((name type))
                                              ((returnName returnType)))
                                             (func (((name1 type1))
                                                    ((returnName1 returnType1))))
                                             (func (((name1 type1))
                                                    ((returnName1 returnType1))))))
                            (list (go:expr
                                   (go:func (cons #f #f)
                                            #f
                                            `((name        . ,(go:type:id 'type       #f)))
                                            `((returnName  . ,(go:type:id 'returnType #f)))
                                            (list (go:expr
                                                   (go:func (cons #f #f)
                                                            #f
                                                            `((name1        . ,(go:type:id 'type1       #f)))
                                                            `((returnName1  . ,(go:type:id 'returnType1 #f)))
                                                            null))
                                                  (go:expr
                                                   (go:func (cons #f #f)
                                                            #f
                                                            `((name1       . ,(go:type:id 'type1      #f)))
                                                            `((returnName1 . ,(go:type:id 'returnType1 #f)))
                                                            null))))))))

               (test-suite "var"
                           (check-equal?
                            (go/expand (var (x y)))
                            (list (go:expr (go:var (list (go:var:binding 'x (go:type:id 'y #f) #f))))))
                           (check-equal?
                            (go/expand (var (x y 1)))
                            (list (go:expr (go:var (list (go:var:binding 'x (go:type:id 'y #f) (go:expr 1)))))))
                           (check-equal?
                            (go/expand (var (x y 1) (xx yy 2)))
                            (list (go:expr (go:var (list (go:var:binding 'x  (go:type:id 'y #f)  (go:expr 1))
                                                         (go:var:binding 'xx (go:type:id 'yy #f) (go:expr 2))))))))

               (test-suite "const"
                           (check-equal?
                            (go/expand (const (x y)))
                            (list (go:expr (go:const (list (go:var:binding 'x (go:type:id 'y #f) #f))))))
                           (check-equal?
                            (go/expand (const (x y 1)))
                            (list (go:expr (go:const (list (go:var:binding 'x (go:type:id 'y #f) (go:expr 1)))))))
                           (check-equal?
                            (go/expand (const (x y 1) (xx yy 2)))
                            (list (go:expr (go:const (list (go:var:binding 'x  (go:type:id 'y #f)  (go:expr 1))
                                                           (go:var:binding 'xx (go:type:id 'yy #f) (go:expr 2))))))))

               (test-suite "go"
                           (check-equal?
                            (go/expand (go (func ())))
                            (list (go:expr (go:go (go:expr (go:func (cons #f #f) #f null null null))))))
                           (check-equal?
                            (go/expand (go ((func ()))))
                            (list (go:expr (go:go (go:expr (go:func:call (go:func (cons #f #f) #f null null null)
                                                                         null)))))))

               (test-suite "if"
                           (check-equal?
                            (go/expand (if (== 1 1) (fmt.Println "ok")))
                            (list (go:expr
                                   (go:if
                                    (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                                    (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                    #f))))
                           (check-equal?
                            (go/expand (if (== 1 1) (fmt.Println "ok") (fmt.Println "not ok")))
                            (list (go:expr
                                   (go:if
                                    (go:expr (go:operator  '==          (list (go:expr 1) (go:expr 1))))
                                    (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                    (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))))
                           (check-equal?
                            (go/expand (if (not (== 1 1)) (fmt.Println "ok") (fmt.Println "not ok")))
                            (list (go:expr
                                   (go:if
                                    (go:expr (go:operator '!
                                                          (list (go:expr
                                                                 (go:operator '==
                                                                              (list (go:expr 1) (go:expr 1)))))))
                                    (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                    (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))))
                           (check-equal?
                            (go/expand (if (not (== (+ 1 5) 1))
                                           (fmt.Println "ok")
                                           (fmt.Println "not ok")))
                            (list (go:expr
                                   (go:if
                                    (go:expr (go:operator '!
                                                          (list (go:expr
                                                                 (go:operator
                                                                  '==
                                                                  (list (go:expr
                                                                         (go:operator '+ (list (go:expr 1) (go:expr 5))))
                                                                        (go:expr 1)))))))
                                    (go:expr (go:func:call 'fmt.Println (list (go:expr "ok"))))
                                    (go:expr (go:func:call 'fmt.Println (list (go:expr "not ok"))))))))
                           (check-equal?
                            (go/expand (if (== 1 1) (begin (fmt.Println "ok"))))
                            (list (go:expr
                                   (go:if
                                    (go:expr (go:operator '== (list (go:expr 1) (go:expr 1))))
                                    (go:expr
                                     (go:begin (list (go:expr (go:func:call 'fmt.Println (list (go:expr "ok")))))))
                                    #f)))))

               (test-suite "when"
                           (check-equal?
                            (go/expand (when #t
                                         (fmt.Println 1)
                                         (fmt.Println 2)))
                            (list (go:expr
                                   (go:if
                                    (go:expr #t)
                                    (go:begin
                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr 1))))
                                           (go:expr (go:func:call 'fmt.Println (list (go:expr 2))))))
                                    #f)))))

               (test-suite "unless"
                           (check-equal?
                            (go/expand (unless #t
                                         (fmt.Println 1)
                                         (fmt.Println 2)))
                            (list (go:expr
                                   (go:if
                                    (go:operator '! (go:expr #t))
                                    (go:begin
                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr 1))))
                                           (go:expr (go:func:call 'fmt.Println (list (go:expr 2))))))
                                    #f)))))

               (test-suite "alias"
                           (check-equal?
                            (go/expand (alias errors New Errorf))
                            (list (go:expr (go:alias "errors" '("New" "Errorf")))))
                           (check-equal?
                            (go/expand (alias errors New (rename (e Errorf))))
                            (list (go:expr (go:alias "errors" '("New" ("e" . "Errorf"))))))
                           (check-equal?
                            (go/expand (alias errors (prefix New (rename (Failure Error)))))
                            (list (go:expr (go:alias "errors" '(("NewFailure" . "Error"))))))
                           (check-equal?
                            (go/expand (alias errors (suffix Example (rename (Failure Error)))))
                            (go/expand (alias errors (rename (FailureExample Error)))))
                           (check-equal?
                            (go/expand (alias xxx Foo Bar (prefix yyy (rename (Baz Qux)))))
                            (list (go:expr (go:alias "xxx" '("Foo" "Bar" ("yyyBaz" . "Qux"))))))
                           (check-equal?
                            (go/expand (alias xxx Foo Bar (suffix yyy Baz Qux)))
                            (list (go:expr (go:alias "xxx" '("Foo" "Bar" ("Bazyyy" . "Baz") ("Quxyyy" . "Qux"))))))
                           (check-equal?
                            (go/expand (alias xxx (const Foo Bar)))
                            (list (go:expr (go:alias "xxx" (list
                                                            (go:alias:const "Foo")
                                                            (go:alias:const "Bar"))))))
                           (check-equal?
                            (go/expand (alias xxx (type Foo Bar)))
                            (list (go:expr (go:alias "xxx" (list
                                                            (go:alias:type "Foo")
                                                            (go:alias:type "Bar"))))))
                           (check-equal?
                            (go/expand (alias xxx (prefix x y z) (type Foo Bar)))
                            (list (go:expr (go:alias "xxx" (list '("xy" . "y")
                                                                 '("xz" . "z")
                                                                 (go:alias:type "Foo")
                                                                 (go:alias:type "Bar"))))))
                           (check-equal?
                            (go/expand (alias xxx
                                              (prefix x y (rename (zz z)))
                                              (type Foo Bar)))
                            (list (go:expr (go:alias "xxx" (list '("xy" . "y")
                                                                 '("xzz" . "z")
                                                                 (go:alias:type "Foo")
                                                                 (go:alias:type "Bar"))))))
                           (check-equal?
                            (go/expand (alias xxx (type Bar (rename (foo Foo)))))
                            (list (go:expr (go:alias "xxx" (list
                                                            (go:alias:type "Bar")
                                                            (go:alias:type '("foo" . "Foo")))))))
                           (check-equal?
                            (go/expand (alias xxx (type (prefix x foo Foo) (prefix y Bar))))
                            (list (go:expr (go:alias "xxx" (list
                                                            (go:alias:type '("xfoo" . "foo"))
                                                            (go:alias:type '("xFoo" . "Foo"))
                                                            (go:alias:type '("yBar" . "Bar"))))))))

               (test-suite "for"
                           (check-equal?
                            (go/expand (for () (fmt.Println k v)))
                            (list (go:expr (go:for
                                            null null #f #f #f
                                            (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))))
                           (check-equal?
                            (go/expand (for ((k v) (create (slice int) (1 2 3))) (fmt.Println k v)))
                            (list (go:expr (go:for
                                            (list 'k 'v)
                                            (list (go:expr
                                                   (go:create
                                                    (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                                            #f #f #f
                                            (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))))
                           (check-equal?
                            (go/expand (for ((k v) (range (create (slice int) (1 2 3)))) (fmt.Println k v)))
                            (list (go:expr (go:for
                                            (list 'k 'v)
                                            (list (go:expr
                                                   (go:create
                                                    (go:type:id 'slice (go:type:id:slice (go:type:id 'int #f)))
                                                    (list (go:expr 1) (go:expr 2) (go:expr 3)))))
                                            #f #f 'range
                                            (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v)))))))))
                           (check-equal?
                            (go/expand (for (k 10 (> k 0) (-- k)) (fmt.Println k)))
                            (list (go:expr (go:for
                                            (list 'k)
                                            (list (go:expr 10))
                                            (go:expr (go:operator '> (list (go:expr 'k) (go:expr 0))))
                                            (go:expr (go:dec 'k))
                                            #f
                                            (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k))))))))))

               (test-suite "begin"
                           (check-equal?
                            (go/expand (begin (fmt.Println k v) (+ 1 2)))
                            (list (go:expr
                                   (go:begin
                                    (list (go:expr (go:func:call 'fmt.Println (list (go:expr 'k) (go:expr 'v))))
                                          (go:expr (go:operator '+ (list (go:expr 1) (go:expr 2))))))))))

               (test-suite "switch"
                           (check-equal?
                            (go/expand (switch 1
                                               (1 (fmt.Println "one"))
                                               (2 (fmt.Println "two"))
                                               (default (fmt.Println "default"))))
                            (list (go:expr
                                   (go:switch (go:expr 1)
                                              (list (go:case
                                                     (go:expr 1)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                                    (go:case
                                                     (go:expr 2)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                                    (go:case
                                                     'default
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default")))))))))))
                           (check-equal?
                            (go/expand (switch (+ 1 1)
                                               (1 (fmt.Println "one"))
                                               (2 (fmt.Println "two"))
                                               (default (fmt.Println "default"))))
                            (list (go:expr
                                   (go:switch (go:expr (go:operator '+ (list (go:expr 1) (go:expr 1))))
                                              (list (go:case
                                                     (go:expr 1)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                                    (go:case
                                                     (go:expr 2)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                                    (go:case
                                                     'default
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default"))))))))))))

               (test-suite "cond"
                           (check-equal?
                            (go/expand (cond (1 (fmt.Println "one"))
                                             (2 (fmt.Println "two"))
                                             (default (fmt.Println "default"))))
                            (list (go:expr
                                   (go:switch null
                                              (list (go:case
                                                     (go:expr 1)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                                    (go:case
                                                     (go:expr 2)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                                    (go:case
                                                     'default
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default")))))))))))
                           (check-equal?
                            (go/expand (cond
                                         (1 (fmt.Println "one"))
                                         (2 (fmt.Println "two"))
                                         (default (fmt.Println "default"))))
                            (list (go:expr
                                   (go:switch null
                                              (list (go:case
                                                     (go:expr 1)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "one"))))))
                                                    (go:case
                                                     (go:expr 2)
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "two"))))))
                                                    (go:case
                                                     'default
                                                     (list (go:expr (go:func:call 'fmt.Println (list (go:expr "default"))))))))))))

               (test-suite "select"
                           (check-equal?
                            (go/expand (select (default (println "default case"))))
                            (list (go:expr
                                   (go:select
                                    (list
                                     (go:case
                                      'default
                                      (list (go:expr (go:func:call 'println (list (go:expr "default case")))))))))))
                           (check-equal?
                            (go/expand (select
                                           ((def x (<- ch)) (fmt.Printf "x: %+v\n" x))
                                         (default         (fmt.Println "default case"))))
                            (list (go:expr
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
                                                   (list (go:expr "default case"))))))))))))
               (test-suite "cast"
                           (check-equal? (go/expand (cast v bool))
                                         (list (go:expr (go:cast (go:expr 'v) (go:type:id 'bool #f)))))
                           (check-equal? (go/expand (cast v (assert bool)))
                                         (list (go:expr (go:cast (go:expr 'v) (go:cast:assert (go:type:id 'bool #f)))))))
               (test-suite "return"
                           (check-equal? (go/expand (return))
                                         (list (go:expr (go:return null))))
                           (check-equal? (go/expand (return (+ 1 1)))
                                         (list (go:expr (go:return
                                                         (list (go:expr
                                                                (go:operator '+ (list
                                                                                 (go:expr 1)
                                                                                 (go:expr 1)))))))))
                           (check-equal? (go/expand (return 1 1))
                                         (list (go:expr (go:return (list (go:expr 1) (go:expr 1)))))))
               (test-suite "break"
                           (check-equal? (go/expand (break))
                                         (list (go:expr (go:break #f))))
                           (check-equal? (go/expand (break xxx))
                                         (list (go:expr (go:break 'xxx)))))
               (test-suite "continue"
                           (check-equal? (go/expand (continue))
                                         (list (go:expr (go:continue #f))))
                           (check-equal? (go/expand (continue xxx))
                                         (list (go:expr (go:continue 'xxx)))))
               (test-suite "spread"
                           (check-equal? (go/expand (spread X))
                                         (list (go:expr (go:spread (go:expr 'X))))))
               (test-suite "label"
                           (check-equal?
                            (go/expand (label xxx (for () (break xxx))))
                            (list (go:expr (go:label 'xxx
                                                     (go:expr (go:for null null #f #f #f
                                                                      (list (go:expr (go:break 'xxx))))))))))
               (test-suite "goto"
                           (check-equal?
                            (go/expand (goto xxx))
                            (list (go:expr (go:goto 'xxx)))))
               (test-suite "iota"
                           (check-equal?
                            (go/expand (iota))
                            (list (go:expr (go:iota)))))
               (test-suite "defer"
                           (check-equal?
                            (go/expand (defer (println "hello")))
                            (list (go:expr (go:defer (go:expr (go:func:call 'println
                                                                            (list (go:expr "hello")))))))))
               (test-suite "slice"
                           (check-equal?
                            (go/expand (slice arr 5 10))
                            (list (go:expr (go:slice (go:expr 'arr)
                                                     (go:expr 5)
                                                     (go:expr 10)))))
                           (check-equal?
                            (go/expand (slice arr 5))
                            (list (go:expr (go:slice (go:expr 'arr)
                                                     (go:expr 5)
                                                     #f))))
                           (check-equal?
                            (go/expand (slice arr 0 10))
                            (list (go:expr (go:slice (go:expr 'arr)
                                                     (go:expr 0)
                                                     (go:expr 10)))))
                           (check-equal?
                            (go/expand (slice arr (+ 1 1) 10))
                            (list (go:expr (go:slice (go:expr 'arr)
                                                     (go:expr (go:operator '+ (list (go:expr 1) (go:expr 1))))
                                                     (go:expr 10))))))

               (test-suite "index"
                           (check-equal?
                            (go/expand (index arr 0))
                            (list (go:expr (go:index (go:expr 'arr) (go:expr 0)))))
                           (check-equal?
                            (go/expand (index arr (* 2 x)))
                            (list (go:expr (go:index (go:expr 'arr)
                                                     (go:expr (go:operator '*
                                                                           (list (go:expr 2) (go:expr 'x)))))))))

               (test-suite "key"
                           (check-equal?
                            (go/expand (key st Foo))
                            (list (go:expr (go:key (go:expr 'st) '(Foo))))))

               (test-suite "send"
                           (check-equal? (go/expand (send ch "test"))
                                         (list (go:expr (go:send (go:expr 'ch)
                                                                 (go:expr "test")))))
                           (check-equal? (go/expand (-> ch "test"))
                                         (list (go:expr (go:send (go:expr 'ch)
                                                                 (go:expr "test"))))))
               (test-suite "receive"
                           (check-equal? (go/expand (receive ch))
                                         (list (go:expr (go:receive (go:expr 'ch)))))
                           (check-equal? (go/expand (<- ch))
                                         (list (go:expr (go:receive (go:expr 'ch))))))
               (test-suite "inc"
                           (check-equal? (go/expand (inc n))
                                         (list (go:expr (go:inc 'n))))
                           (check-equal? (go/expand (++ n))
                                         (list (go:expr (go:inc 'n)))))
               (test-suite "dec"
                           (check-equal? (go/expand (dec n))
                                         (list (go:expr (go:dec 'n))))
                           (check-equal? (go/expand (-- n))
                                         (list (go:expr (go:dec 'n)))))
               (test-suite "ref"
                           (check-equal? (go/expand (ref v))
                                         (list (go:expr (go:ref (go:expr 'v))))))
               (test-suite "deref"
                           (check-equal? (go/expand (deref v))
                                         (list (go:expr (go:deref (go:expr 'v))))))

               ;; other expression cases

               (test-suite "primitive"
                           (test-case "nil"
                             (check-equal? (go/expand nil)                (list (go:expr 'nil))))
                           (test-case "bool"
                             (check-equal? (go/expand #t)                 (list (go:expr #t)))
                             (check-equal? (go/expand #f)                 (list (go:expr #f))))
                           (test-case "number"
                             (check-equal? (go/expand 666)                (list (go:expr 666)))
                             (check-equal? (go/expand 666.6)              (list (go:expr 666.6)))
                             (check-equal? (go/expand -666)               (list (go:expr -666)))
                             (check-equal? (go/expand -666.6)             (list (go:expr -666.6))))
                           (test-case "string"
                             (check-equal? (go/expand "hello")            (list (go:expr "hello"))))
                           (test-case "identifier"
                             (check-equal? (go/expand runtime.GOMAXPROCS) (list (go:expr 'runtime.GOMAXPROCS)))))

               (test-suite "dummy"
                           (check-equal? (go/expand (package main)
                                                    (import os fmt)
                                                    (func (main () ()) (println os.Args)))
                                         (list (go:expr (go:package 'main))
                                               (go:expr (go:imports (list (go:import 'os #f) (go:import 'fmt #f))))
                                               (go:expr (go:func (cons #f #f) 'main null null
                                                                 (list (go:expr (go:func:call 'println
                                                                                              (list (go:expr 'os.Args))))))))))

               (test-suite "complex"
                           (test-case "cli"
                             (check-equal?
                              (go/expand (package main)
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

                              (list (go:expr (go:package 'main))
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
                                    (go:expr (go:func (cons #f #f)
                                                      'RootAction
                                                      (list (cons 'ctx (go:type:id 'ptr (go:type:id:ptr (go:type:id 'cli.Context #f)))))
                                                      (list (go:type:id 'error #f))
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
                                                                                                  (go:type:id:ptr (go:type:id 'cli.App #f)))
                                                                                      null)))))
                                                            (go:expr (go:set (list (go:expr 'app.Flags))
                                                                             (list (go:expr 'Flags))))
                                                            (go:expr (go:set (list (go:expr 'app.Action))
                                                                             (list (go:expr 'RootAction))))
                                                            (go:expr (go:func:call 'app.Run
                                                                                   (list (go:expr 'os.Args))))))))))))))
    (displayln (format "test suite ~s" (rackunit-test-suite-name suite)))
    (display "\t")
    (run-tests suite 'verbose)))
