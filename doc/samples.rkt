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

(group "packages"
       (example (import fmt github.com/pkg/errors)
                (func (main)
                      (fmt.Println (errors.New "hello"))))
       (example (import (log github.com/rs/zerolog))
                (func (main)
                      (log.Print "hello"))))

(group "operators"
       (group "math"
              (example (import fmt)
                       (func (main)
                             (fmt.Println
                              (+ 1 1)
                              (- 1 1)
                              (% 11 10)
                              (* 1 1)
                              (/ 2 2))
                             (def x 0)
                             (inc x)
                             (fmt.Println "x =" x)
                             (dec x)
                             (fmt.Println "x =" x))))
       (group "boolean"
              (example (import fmt)
                       (func (main)
                             (fmt.Println
                              (== 1 1)
                              (!= 2 1)
                              (! (== 2 1))
                              (not (== 2 1))
                              (> 2 1)
                              (>= 1 1)
                              (< 1 2)
                              (<= 1 1)
                              (and (== 1 1) (== 2 2))
                              (or (== 1 2) (== 1 3) (== 1 1))
                              (bitwise-and 1 1 1) ;; TODO: make a note about and&or and racket(why not |&)
                              (bitwise-or 1 1)
                              (bitwise-xor 1 0 0)
                              (^ 1 0 0)
                              (bitwise-left-shift 1 2)
                              (<< 1 2 3)
                              (bitwise-right-shift 2 1)
                              (>> 10 1 2))))))

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
       (example (def strings (create (slice string) ("hello" "world")))
                (println (slice strings 1))
                (println (+ (index strings 1) ", " (index strings 0))))
       (example (def stringsArray (create (array string ...) ("hello" "world")))
                (println (index stringsArray 1))))

(group "functions"
       (group "named and anonymous"
              (example (func (sayHello ((name string)))
                             (println (+ "hello " name)))
                       (func (main) (sayHello "golisp")))
              (example (func (returnHello ((name string)) (string))
                             (return (+ "hello " name)))
                       (func (main) (println (returnHello "golisp"))))
              (example (func (main)
                             ((func () (println "hello golisp"))))))

       (group "spread"
              (example (import strings)
                       (func (sayHello ((&rest names string)))
                             (println (+ "hello, " (strings.Join names " "))))
                       (func (main)
                             (sayHello (spread (create (slice string)
                                                       ("my" "name" "is" "boris")))))))

       (group "go & defer"
              (example (import sync)
                       (func (main)
                             (def wg (create sync.WaitGroup))
                             (wg.Add 1)
                             (go ((func ()
                                        (println "hello golisp")
                                        (wg.Done))))
                             (wg.Wait)))
              (example (func (main)
                             (defer (println "the execution order is..."))
                             (println "reversed"))))

       (group "struct methods"
              (example (type (Greet (struct)))
                       (func ((sayHello (Greet)) ((name string)))
                             (println (+ "hello " name)))
                       (func (main) ((key (create Greet) sayHello) "golisp")))))

(group "structs"
       (example (import fmt sync)
                (type (State (struct
                               sync.RWMutex
                               (store (map string (interface))))))
                (func ((set (s (ptr State)))
                       ((key string) (value (interface)))
                       ((ptr State)))
                      (s.Lock)
                      (defer (s.Unlock))
                      (set (index s.store key) value)
                      (return s))
                (func ((get (s (ptr State)))
                       ((key string))
                       ((value (interface)) (ok bool)))
                      (s.RLock)
                      (defer (s.RUnlock))
                      (set (value ok) ((index s.store key)))
                      (return))
                (func (main)
                      (def state (create State ((store (create (map string (interface)))))))
                      (state.set "foo" "bar")
                      (def (value ok) ((state.get "foo")))
                      (fmt.Println "has value?" ok)
                      (fmt.Println value)))
       (example (import fmt encoding/json)
                (type (Payload (struct
                                 (Body (slice byte) (tag (json "body"))))))
                (func (main)
                      (def (buf err)
                        ((json.Marshal
                          (create Payload ((Body (cast "hello world" (slice byte))))))))
                      (when (!= err nil) (panic err))
                      (fmt.Println (cast buf string)))))

(group "interfaces"
       (example (type (Greeter (interface
                                   (Greet (func (((name string))))))))
                (type (HelloGreeter (struct)))
                (func ((Greet (HelloGreeter)) ((name string)))
                      (println "hello" name))
                (func (main)
                      (var (g Greeter (create HelloGreeter)))
                      (g.Greet "world"))))

(group "channels"
       (example (var (ch (chan string) (make (type chan string) 5)))
                (func (main)
                      (send ch "hello")
                      (-> ch "world")
                      (println (receive ch))
                      (println (<- ch)))))

(group "loops"
       (example (import fmt)
                (func (main)
                      (def values (create (slice int) (1 2 3)))
                      (for ((k v) (range values))
                        (fmt.Println k v))))
       (example (import fmt)
                (func (main)
                      (for (k 10 (> k 0) (-- k))
                        (fmt.Println k))))
       (example (import fmt)
                (func (main)
                      (for (k 10 (> k 0) (-- k))
                        (when (< k 5) (break))
                        (fmt.Println k))))
       (example (import fmt)
                (func (main)
                      (for (k 10 (> k 0) (-- k))
                        (when (> k 5) (continue))
                        (fmt.Println k)))))

(group "flow control"
       (example (func (main)
                      (if true (println "thats true"))
                      (if false
                          (println "thats true")
                          (println "ain't done this"))))
       (example (func (main)
                      (when true (println "thats true"))
                      (unless false
                        (println "ain't done this"))))
       (example (import runtime)
                (func (main)
                      (def os (key runtime GOOS))
                      (switch os
                              ("darwin" (println "os x"))
                              ("linux"  (println "linux"))
                              (default  (println os)))))
       (example (import time)
                (func (main)
                      (def t (time.Now))
                      (cond ((< (t.Hour) 12) (println "good morning!"))
                            ((< (t.Hour) 17) (println "good afternoon!"))
                            (default         (println "good evening!")))))
       (example (import fmt)
                (var ((ch)   (make (type (chan string))))
                     ((done) (make (type (chan (struct))))))
                (func (main)
                      (go ((func ()
                                 (defer (close done))
                                 (send ch "hello"))))
                      (label loop
                             (for ()
                               (select
                                ((def v (receive ch))
                                 (fmt.Println "got a message" v))
                                ((receive done)
                                 (fmt.Println "finished")
                                 (break loop)))))))
       (example (func (main)
                      (def n 0)
                      (label start
                             (begin
                               (println n)
                               (inc n)
                               (when (< n 5) (goto start)))))))

(group "macro"
       (example (import fmt)
                (macro (inspect (x)) `(fmt.Printf "%#v\n" ,x))
                (func (main) (inspect 5)))
       (example (import fmt)
                (macro (format (f v)) `(fmt.Printf ,f ,v))
                (macro (inspect (x)) `(format "%#v\n" ,x))
                (func (main) (inspect 5))))
