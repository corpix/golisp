#lang racket/base
(require racket/syntax
         "../main.rkt"
         (for-syntax racket/base))


(define-syntax (group stx)
  (syntax-case stx ()
    ((_ title xs ...)
     (syntax (list title xs ...))))) ;; NOTE: this thing to write a group header

(define-syntax (example stx)
  (syntax-case stx ()
    ((_ xs ...) (syntax (cons (quote (xs ...))
                              (go/string (package main) xs ...))))))

;; functions (func return spread go defer)
;; structs
;; interfaces
;; channels (<- -> send receive)
;; loops (for break continue)
;; flow control (if switch select goto label)


(group "packages"
       (example (import github.com/pkg/errors
                        google.golang.org/grpc))
       (example (import (log github.com/rs/zerolog))))

(group "operators"
       (group "math"
              (example (func (main)
                             (create (slice int)
                                     ((+ 1 1)
                                      (- 1 1)
                                      (% 11 10)
                                      (* 1 1)
                                      (/ 2 2))))))
       (group "boolean"
              (example (== 1 1))
              (example (!= 2 1))
              (example (! (== 2 1)))
              (example (not (== 2 1)))
              (example (> 2 1))
              (example (>= 1 1))
              (example (< 1 2))
              (example (<= 1 1))
              (example (and (== 1 1) (== 2 2)))
              (example (or (== 1 2) (== 1 3) (== 1 1)))
              (example (bitwise-and 1 1 1)) ;; TODO: make a note about and&or and racket(why not |&)
              (example (bitwise-or 1 1))
              (example (bitwise-xor 1 0 0))
              (example (^ 1 0 0))
              (example (bitwise-left-shift 1 2))
              (example (<< 1 2 3))
              (example (bitwise-right-shift 2 1))
              (example (>> 10 1 2))))

(group "variables"
       (example (var (name string "golisp")
                     (version int))
                (func (main) (println "using" name version)))
       (example (var ((name) "golisp")
                     ((version int64) 1))
                (func (main) (println "using" name version)))
       (example (const ((foo) "bar"))
                (func (main) (println foo))) ;; FIXME: const could have just type without initialization, thats wrong
       (example (func (main)
                      (def hello "world")
                      (set hello "golisp")
                      (println hello))))

(group "types"
       (group "declaration"
              (example (type (alias Name string))
                       (var (name Name "boris"))
                       (func (main) (println "hello, my name is" name)))
              (example (import time)
                       (type (alias Time time.Time)
                             (Tasks (map Time (slice string))))
                       (func (main)
                             (def now (time.Now))
                             (def (current ok) (index Tasks now))
                             (if (not ok) (println "will create new bucket"))
                             (set (index Tasks now)
                                  (append current "new")))))
       (group "initialization"
              (example (def foo (create string "foo")))
              (example (def bar (create (map int string) ((0 "hello")
                                                          (1 "world")))))
              (example (def baz (create (slice string) ("hello" "world"))))
              (example (def qux (create (array string 2) ("hello" "world"))))))

(group "slices and arrays"
       (example (type (slice string)))
       (example (def strings (slice string) ("hello" "world"))
                (println (slice strings 1))
                (println (+ (index strings 1) ", " (index strings 0))))
       (example (def stringsArray (create (array string ...) ("hello" "world")))
                (println (index stringsArray 1))))

(group "functions"
       (group "named and anonymous"
              (example (func (sayHello ((name string)))
                             (println (+ "hello " name))))
              (example (func (returnHello ((name string)) (string))
                             (return (+ "hello " name))))
              (example ((func ()
                              (println "hello lambda world")))))

       (group "spread"
              (example (import strings)
                       (func (sayHello ((&rest names string)))
                             (println (+ "hello, " (strings.Join names " "))))
                       (func (main)
                             (sayHello (spread (create (slice string)
                                                       ("my" "name" "is" "boris")))))))

       (group "struct methods"
              (example (type (Greet (struct)))
                       (func ((sayHello (g Greet)) ((name string))) ;; FIXME: make receiver name optional
                             (println (+ "hello " name))))))

(group "properties"
       (example (println "hello"))
       (example (import fmt)
                ((key fmt Println) "hello"))
       (example (def example (create (struct (level1 (struct (level2 string))))
                                     ((create (struct (level2 string)) ("nested property")))))
                (println (key example level1 level2))
                (set (key example level1 level2) "modified nested property")))

(group "structs"
       (example (type (Language (struct
                                  (name string)
                                  (version int)))))
       (example (def lang (create Language ("golisp" 1))))
       (example (set (key lang version) 2)))
