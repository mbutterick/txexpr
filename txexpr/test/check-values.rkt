#lang racket/base

(provide values->list
         check-values=?
         check/values)

(require rackunit
         (for-syntax racket/base))

;; Takes an expression producing an unknown number of values
;; and wraps them all in a list.
(define-syntax-rule (values->list values-expr)
  (call-with-values (λ () values-expr) list))


;; Checks that two expressions (each producing an unknown
;; number of values until evaluated) produce the same number
;; of values, and that the values are equal.
(define-syntax check-values=?
  (lambda (stx)
    (syntax-case stx []
      [(_ actual-expr expected-expr)
       (syntax/loc stx
         (check/values check-equal? actual-expr expected-expr))])))

(define-syntax check/values
  (lambda (stx)
    (syntax-case stx []
      [(_ check-form actual-expr expected-expr)
       (syntax/loc stx
         (check-form (vs actual-expr) (vs expected-expr)))])))


;; Takes an expression producing an unknown number of values
;; and wraps them in a "fake-values" structure that can be
;; compared with other fake-values structures for equality,
;; and can be printed to look like a call to `values`.
(define-syntax-rule (vs values-expr)
  (fake-values (values->list values-expr)))

;; if make-constructor-style-printer from racket/struct exists,
;; this is it, and otherwise this is a cheap immitation.
(define make-constructor-style-printer
  (with-handlers ([exn:fail:filesystem?
                   (λ (e)
                     (λ (get-head get-elems)
                       (λ (v out mode)
                         (define head (get-head v))
                         (define elems (get-elems v))
                         (fprintf out "(~a" head)
                         (for ([elem elems])
                           (fprintf out " ~v" elem))
                         (fprintf out ")"))))])
    (dynamic-require 'racket/struct 'make-constructor-style-printer)))

(struct fake-values [list]
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (self) 'values)
      (lambda (self) (fake-values-list self))))])

