#lang racket/base
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(struct go:package   (name)    #:prefab)

(struct go:imports   (imports)         #:prefab)
(struct go:import    (package altname) #:prefab)
