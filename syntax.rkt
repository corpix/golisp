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
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     racket/list
                     "type.rkt"))

(provide (all-defined-out)
         (for-syntax (all-defined-out))
         (all-from-out "type.rkt"))

;;

(define +new-line+ "\n")
(define +tab+ "\t")
(define +comma+ ", ")
(define +set+ " = ")
(define +space+ " ")

(define *macro* (make-parameter (make-hasheq)))

;;

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

;;

(define (~symbol->string v)
  (match v
    ((? symbol?  v) (symbol->string v))
    ((? string?  v)  v)
    ((? false?   v) "")))

(define ((partial f . xs) . xxs)
  (apply f (append xs xxs)))

(module+ test
  (require rackunit)

  (define (+* x y) (+ x y))
  (check-equal? ((partial +*) 1 2) 3)
  (check-equal? ((partial +* 1) 2) 3)
  (check-equal? ((partial +* 1 2)) 3))

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
                          (lambda (caller-args)
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
          (string-append (~symbol->string altname)
                         +space+)
          "")
      (~s package)))))

(define (emit-bindings ast)
  (displayln ast)
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
                              (emit (expand value))))
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

(define (emit-func ast)
  (match ast
    ((go:func name args return body)
     (string-append "func" +space+
                    (~symbol->string name) (emit-bindings (or args null))
                    +space+                (emit-bindings (or return null))
                    +space+ "{" +new-line+ (emit (expand body)) +new-line+ "}"))))

(define (emit-var ast)
  (match ast
    ((go:var bindings)
     (string-append "var" +space+ (emit-bindings bindings)))))

(define (emit-id ast)
  (~a ast))

(define (emit-string ast)
  (~s ast))

(define (emit-number ast)
  (~a ast))

(define (emit ast)
  (match ast
    ((? go:package?      ast) (emit-package       ast))
    ((? go:imports?      ast) (emit-imports       ast))
    ((? go:func?         ast) (emit-func          ast))
    ((? go:var?          ast) (emit-var           ast))
    ((? go:binding?      ast) (emit-bindings      ast))


    ((? symbol? ast)  (emit-id (symbol->string ast)))
    ((? string? ast)  (emit-string ast))
    ((? number? ast)  (emit-number ast))

    ((? (lambda (v) (and (list? v)
                         (not (empty? v))
                         (symbol? (car v))))
        ast)
     (format "~a(~a)"
             (car ast)
             (string-join (map emit (cdr ast))
                          +comma+)))

    ((? list? ast)
     (string-join
      (map emit ast)
      +new-line+))
    ))

(define (expand ast)
  (let ((macro?
         (lambda (v)
           (and (list? v)
                (not (empty? v))
                (symbol? (car v))
                 (hash-has-key? (*macro*) (car v))))))
    (match ast
      ((? struct? ast) ast)

      ((? macro? ast)
       ((hash-ref (*macro*) (car ast)) (cdr ast)))

      ((? list? ast) (map expand ast))
      (_ ast))))

;; (define-syntax (: stx)
;;   (syntax-parse stx
;;     ((_ xs:Colon) #`(go:group (quasiquote #,(attribute xs.ast))))))

;;

(begin-for-syntax
  (define-syntax-class Package
    #:description "current package name, like: main"
    #:attributes (ast)
    (pattern name:id #:attr ast (go:package (syntax->datum #'name))))

  (define-syntax-class Import
    #:description "list of package imports, like: \"github.com/urfave/cli\""
    #:attributes (ast)
    (pattern pkg:string
             #:attr ast
             (go:import (syntax->datum #'pkg) #f)))
  (define-syntax-class ImportNamed
    #:description "list of named package imports, like: (c \"github.com/urfave/cli\")"
    #:attributes (ast)
    (pattern (altname:id pkg:string)
             #:attr ast
             (go:import
              (syntax->datum #'pkg)
              (syntax->datum #'altname))))
  (define-splicing-syntax-class Imports
    #:description "list of package imports"
    #:attributes (ast)
    (pattern (~seq (~or* v:ImportNamed v:Import) ...)
             #:attr ast
             (go:imports (attribute v.ast))))

  (define-splicing-syntax-class Bindings
    #:description "name to type/name to type and value binding"
    #:attributes (ast)
    (pattern (~seq ((~optional var:id #:defaults ((var #'#f))) type:id) ...)
             #:attr ast
             (map (lambda (n t) (apply go:binding (map syntax->datum (list n t #'#f))))
                  (if (empty? (syntax->list #'(var ...)))
                      (vector->list (make-vector (length (syntax->list #'(type ...))) #'#f))
                      (syntax->list #'(var ...)))
                  (syntax->list #'(type ...))))
    (pattern (~seq ((var:id (~optional type:id #:defaults ((type #'#f))) value)) ...)
             #:attr ast
             (map (lambda (n t v) (apply go:binding (map syntax->datum (list n t v))))
                  (syntax->list #'(var ...))
                  (if (empty? (syntax->list #'(type ...)))
                      (vector->list (make-vector (length (syntax->list #'(type ...))) #'#f))
                      (syntax->list #'(type ...)))
                  (syntax->list #'(value ...)))))

  (define-splicing-syntax-class Func
    #:description "function"
    #:attributes (ast)
    (pattern (~seq ((~optional  name:id       #:defaults ((name #'#f)))
                    (~optional (tb:Bindings)  #:defaults ((tb   #'()))) ;; FIXME: this allows `foo string = "something"` in func arg declaration
                    (~optional (rtb:Bindings) #:defaults ((rtb  #'()))))
                   body:expr ...)
             #:attr ast
             (go:func (syntax->datum #'name)
                      (attribute tb.ast)
                      (attribute rtb.ast)
                      (map syntax->datum (syntax->list #'(body ...))))))

  (define-splicing-syntax-class Var
    #:description "variable definition"
    #:attributes (ast)
    (pattern var:Bindings
             #:attr ast
             (go:var (attribute var.ast)))))

;;

(define-gosyntax (package stx)
  (syntax-parse stx
    ((_ pkg:Package) #`(quasiquote #,(attribute pkg.ast)))))

(define-gosyntax (import stx)
  (syntax-parse stx
    ((_ imports:Imports) #`(quasiquote #,(attribute imports.ast)))))

(define-gosyntax (func stx)
  (syntax-parse stx
    ((_ func:Func) #`(quasiquote #,(attribute func.ast)))))

(define-gosyntax (var stx)
  (syntax-parse stx
    ((_ var:Var) #`(quasiquote #,(attribute var.ast)))))
