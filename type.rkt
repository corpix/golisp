#lang racket/base
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(struct go:operator (id operands) #:prefab)

(struct go:type                 (kind parameters) #:prefab)
(struct go:type:map             (key value)       #:prefab)
(struct go:type:struct          (fields)          #:prefab)
(struct go:type:struct:field    (name type tag)   #:prefab)
(struct go:type:interface       (fields)          #:prefab)
(struct go:type:interface:field (name type)       #:prefab)
(struct go:type:slice           (type)            #:prefab)
(struct go:type:array           (type size)       #:prefab)
(struct go:type:ptr             (type)            #:prefab)
(struct go:type:chan            (direction type)  #:prefab)
(struct go:type:func            (input output)    #:prefab)

(struct go:instance (type value) #:prefab)

;;

(struct go:def (id expr) #:prefab)
(struct go:set (id expr) #:prefab)

;;

(struct go:package (name) #:prefab)

(struct go:imports (imports)         #:prefab)
(struct go:import  (package altname) #:prefab)

(struct go:func (name input output body) #:prefab)

(struct go:var         (bindings)        #:prefab)
(struct go:var-binding (name type value) #:prefab)
(struct go:const       (bindings)        #:prefab)

;;

(struct go:expr (exprs) #:prefab)
