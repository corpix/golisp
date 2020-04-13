#lang racket/base
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(struct go:operator (id operands) #:prefab)

(struct go:instance (type value) #:prefab)

(struct go:def (id expr) #:prefab)
(struct go:set (id expr) #:prefab)

(struct go:type                    (value)           #:prefab)
(struct go:type:id:map             (key value)       #:prefab)
(struct go:type:id:struct          (fields)          #:prefab)
(struct go:type:id:struct:field    (name type tag)   #:prefab)
(struct go:type:id:interface       (fields)          #:prefab)
(struct go:type:id:interface:field (name type)       #:prefab)
(struct go:type:id:slice           (type)            #:prefab)
(struct go:type:id:array           (type size)       #:prefab)
(struct go:type:id:ptr             (type)            #:prefab)
(struct go:type:id:chan            (direction type)  #:prefab)
(struct go:type:id:func            (input output)    #:prefab)
(struct go:type:id                 (name parameters) #:prefab)

;;

(struct go:package (name) #:prefab)

(struct go:imports (imports)         #:prefab)
(struct go:import  (package altname) #:prefab)

(struct go:func      (name input output body) #:prefab)
(struct go:func:call (func arguments)         #:prefab)

(struct go:var         (bindings)        #:prefab)
(struct go:var:binding (name type value) #:prefab)
(struct go:const       (bindings)        #:prefab)

(struct go:go  (func)                #:prefab)
(struct go:if  (condition then else) #:prefab)
(struct go:for (vars seq body)       #:prefab)

;;

(struct go:expr (exprs) #:prefab)
