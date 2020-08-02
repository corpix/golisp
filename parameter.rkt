#lang racket/base
(require "type.rkt")
(provide (all-defined-out))

(define *transform-depth*    (make-parameter 16))
(define *transformers*       (make-parameter null))
(define *phases*             (make-parameter '(define expand)))
(define *macro*              (make-parameter (make-hasheq)))
(define *prelude*            (make-parameter (box null)))
(define *epilogue*           (make-parameter (box null)))
