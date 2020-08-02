#lang racket/base
(require racket/match
         racket/string
         "type.rkt")

(provide ast->gisp)

(define (emit-operator ast)
  (match ast
    ((go:operator id operands) `(,id ,@operands))))

(define (emit-type ast)
  (match ast
    ((go:type name value alias?)
     `(type ,@(if alias? '(alias) null)
            ,@(if name (list name) null)
            ,(emit-type value)))
    ((go:type:id name type)
     `(emit-type ))))

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
    ((? go:macro?    ast) (emit-macro    ast))
    ((? go:quote?    ast) (emit-quote    ast))
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
    ((? list? ast)   (string-join (map emit-expr ast) +new-line+))))


(define ast->gisp emit-expr)

(module+ test
  (require rackunit
           "tool.rkt")

  (run-suites
   (list
    (test-suite "operator"))))
