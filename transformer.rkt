#lang racket/base
(require racket/struct
         racket/function
         syntax/parse
         "syntax.rkt"
         "type.rkt"
         "tool.rkt"
         "parameter.rkt"
         "transformer-package.rkt"
         "transformer-macro.rkt"
         (for-syntax racket/base
                     syntax/parse))
(provide walk
         make-transformers
         transformers-flatten
         transformer-ref
         phase-ref

         go/with-env
         go/transform-once
         go/transform

         (all-from-out "transformer-package.rkt")
         (all-from-out "transformer-macro.rkt"))

(define-namespace-anchor ns-anchor)
(define ns (namespace-anchor->namespace ns-anchor))

(define (walk proc xs)
  (let* ((pos   0)
         (arity (procedure-arity proc))
         (proc* (cond ((arity-includes? arity 2) (lambda (node pos) (proc node pos)))
                      ((arity-includes? arity 1) (lambda (node _)   (proc node)))
                      (#t (error (format "walk: expected proc with arity 1 or 2, got ~a"
                                         arity))))))
    (let loop ((node xs))
      (let ((result (proc* node pos)))
        (when (void? result) (set! result node))
        (set! pos (add1 pos))
        (cond
          ((walk:skip? result) (walk:skip-unbox result))
          ((prefab-struct-key result)
           (apply make-prefab-struct
                  (prefab-struct-key result)
                  (map loop (struct->list result))))
          ((list? result)
           (map loop result))
          (#t result))))))

(define (make-transformers)
  (list (make-package-transformer)
        (make-macro-transformer)))

(define (transformers-flatten transformers)
  (for/fold ((acc null))
            ((transformer (in-list transformers)))
    (append acc (map (lambda (proc) (cons (transformer-name transformer) proc))
                     (transformer-procs transformer)))))

(define (transformer-ref transformers name (default (void)))
  (or (findf (lambda (transformer)
               (eq? (transformer-name transformer)
                    name))
             transformers)
      (begin0 default
        (when (eq? default (void))
          (error (format "transformer-ref: no value found for key: ~e" name))))))

(define (phase-ref transformer phase)
  (for/fold ((acc null))
            ((proc (in-list (transformer-procs transformer))))
    (if (eq? (proc-phase proc) phase)
        (append (list proc) acc)
        acc)))

;;

(define-syntax (go/with-env stx) ;; FIXME: should copy current env
  (syntax-parse stx
    ((_ ((binding:id e:expr) ...) xs:expr ...+)
     (let* ((initial '((*macro*        (make-hasheq))
                       (*transformers* (make-transformers))
                       (*prelude*      (box null))
                       (*epilogue*     (box null))))
            (providen (attribute binding))
            (merged   (append (filter (lambda (kv) (not (member (car kv) providen)))
                                      initial)
                              (map list
                                   (attribute binding)
                                   (attribute e)))))
       (quasisyntax (parameterize ((unsyntax-splicing merged)) (begin xs ...)))))))

(define-syntax (go/transform-once stx)
  (syntax-parse stx
    ((_ xs:expr ...+)
     (syntax
      (let ((transformers (transformers-flatten (*transformers*)))
            (acc (go/expand-syntax xs ...)))
        (for ((phase (in-list (*phases*))))
          (for ((transformer (in-list transformers)))
            (let ((proc (cdr transformer)))
              (when (eq? phase (proc-phase proc))
                (set! acc (walk (proc-value proc) acc))))))
        acc)))))

(define-syntax (go/transform stx)
  (syntax-parse stx
    ((_ xs:expr ...+)
     (syntax
      (let loop ((counter 0)
                 (curr    (quote (xs ...)))
                 (prev    null))
        (cond
          ((>= counter (*transform-depth*))
           (error (format "*transform-depth* limit reached, counter ~a" counter)))
          ((not (equal? prev curr))
           (loop (add1 counter)
                 (eval `(go/transform-once ,@curr) ns)
                 curr))
          (#t curr)))))))

(module+ test
  (require rackunit
           "tool.rkt")

  (struct sample (v) #:prefab)

  (run-suites
   (list (test-suite "walk"
                     (check-equal? (walk void         '(1 2 3 (4 (5)))) '(1 2 3 (4 (5))))
                     (check-equal? (walk (lambda _ 1) '(1 2 3 (4 (5)))) 1)
                     (check-equal? (walk (lambda (node pos)
                                           (cond
                                             ((number? node)
                                              (+ 1 node))
                                             ((and (> pos 0)
                                                   (list? node))
                                              (walk:skip node))
                                             (#t node)))
                                         '(1 2 3 (4 (5))))
                                   '(2 3 4 (4 (5))))
                     (let* ((buf null)
                            (save (lambda (node)
                                    (begin0 node
                                      (set! buf (cons node buf))))))
                       (walk save '(foo (bar (#s(sample (baz qux))))))
                       (check-equal? buf
                                     '(qux
                                       baz
                                       (baz qux)
                                       #s(sample (baz qux))
                                       (#s(sample (baz qux)))
                                       bar
                                       (bar (#s(sample (baz qux))))
                                       foo
                                       (foo (bar (#s(sample (baz qux))))))))
                     (let* ((buf null)
                            (save (lambda (node pos)
                                    (begin0 node
                                      (set! buf (cons (cons node pos) buf))))))
                       (walk save '(foo (bar (#s(sample (baz qux))))))
                       (check-equal? buf
                                     '((qux . 8)
                                       (baz . 7)
                                       ((baz qux) . 6)
                                       (#s(sample (baz qux)) . 5)
                                       ((#s(sample (baz qux))) . 4)
                                       (bar . 3)
                                       ((bar (#s(sample (baz qux)))) . 2)
                                       (foo . 1)
                                       ((foo (bar (#s(sample (baz qux))))) . 0)))))
         (test-suite "transfomers-flatten"
                     (check-equal?
                      (transformers-flatten
                       (list (transformer
                              'test
                              (list
                               (proc 'foo 'foo-proc)
                               (proc 'bar 'bar-proc)))
                             (transformer
                              'test2
                              (list
                               (proc 'foo 'foo-proc)
                               (proc 'bar 'bar-proc)))))
                      (list
                       (cons 'test  (proc 'foo 'foo-proc))
                       (cons 'test  (proc 'bar 'bar-proc))
                       (cons 'test2 (proc 'foo 'foo-proc))
                       (cons 'test2 (proc 'bar 'bar-proc)))))
         (test-suite "transformer-ref"
                     (check-equal?
                      (transformer-ref
                       (list (transformer 'package
                                          (list
                                           (proc 'define 'do-define)
                                           (proc 'expand 'do-expand))))
                       'package)
                      (transformer
                       'package
                       (list (proc 'define 'do-define) (proc 'expand 'do-expand)))))
         (test-suite "phase-ref"
                     (check-equal?
                      (phase-ref (transformer
                                  'package
                                  (list
                                   (proc 'define 'do-define)
                                   (proc 'expand 'do-expand)))
                                 'define)
                      (list (proc 'define 'do-define))))
         (test-suite "go/transform-once"
                     (parameterize ((*transformers* (make-transformers)))
                       (check-equal?
                        (begin
                          (go/transform-once (macro (x (y)) `(+ 1 ,y))
                                             (x 2)))
                        '(#s(go:expr #s(go:macro x (y) (`(+ 1 ,y))))
                          #s(go:expr #s(go:operator + (#s(go:expr 1) #s(go:expr 2)))))))
                     (parameterize ((*transformers* (make-transformers)))
                       (check-equal?
                        (go/transform-once (macro (z (c y)) `(+ ,c ,y))
                                           (macro (x (y))   `(z  1 ,y))
                                           (x 2))
                        '(#s(go:expr #s(go:macro z (c y) (`(+ ,c ,y))))
                          #s(go:expr #s(go:macro x (y)   (`(z  1 ,y))))
                          #s(go:expr #s(go:func:call z (#s(go:expr 1) #s(go:expr 2))))))))
         (test-suite "go/transform"
                     (parameterize ((*transformers* (make-transformers)))
                       (check-equal?
                        (go/transform (macro (z (c y)) `(+ ,c ,y))
                                      (macro (x (y))   `(z  1 ,y))
                                      (x 2))
                        '(#s(go:expr #s(go:macro z (c y) (`(+ ,c ,y))))
                          #s(go:expr #s(go:macro x (y)   (`(z  1 ,y))))
                          #s(go:expr #s(go:operator + (#s(go:expr 1) #s(go:expr 2)))))))))))
