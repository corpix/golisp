#lang racket/base
(require "syntax.rkt"
         "expand.rkt"
         "ast-go.rkt")
(provide go/expand
         go/expand-syntax
         go/expand-macro
         go/define-macro
         go/string
         go/write-file
         *prelude*
         *epilogue*
         *scope*
         ast->go)
