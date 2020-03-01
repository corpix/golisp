#lang racket/base
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(struct go:package   (name)    #:prefab)

(struct go:imports   (imports)         #:prefab)
(struct go:import    (package altname) #:prefab)

(struct go:func    (name args return body) #:prefab)
(struct go:var     (bindings)              #:prefab)
(struct go:binding (name type value)       #:prefab)
