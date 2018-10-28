#lang racket/base

(define-syntax-rule (r+p MODNAME ...)
  (begin
    (begin
      (require MODNAME)
      (provide (all-from-out MODNAME))
      (module+ safe
        (require (submod MODNAME safe))
        (provide (all-from-out (submod MODNAME safe))))) ...))

(r+p "base.rkt"
     "stx.rkt"
     "check.rkt")