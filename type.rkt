#lang racket/base
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(struct go:package   (name)    #:prefab)
(struct go:import    (imports) #:prefab)
