#lang racket/base
(require "type.rkt"
         "parameter.rkt")
(provide make-package-transformer)

(define (do-define node) (walk:skip node))
(define (do-expand node) (walk:skip node))

(define (make-package-transformer)
  (transformer 'package
               (map proc
                    `(define expand)
                    `(,do-define ,do-expand))))
