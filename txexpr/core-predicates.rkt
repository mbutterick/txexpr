#lang racket/base
(require sugar/define xml racket/match)
(provide (all-defined-out) valid-char? cdata? cdata xexpr->string xexpr?)

(define (txexpr-short? x)
  (match x
    [(list (? symbol? name) (? xexpr?) ...) #t]
    [else #f]))

(define (txexpr? x)
  (or (txexpr-short? x)
      (match x
        [(list (? symbol?) (list (list (? symbol?) (? string?)) ...) (? xexpr?) ...) #t]
        [else #f])))

(define (txexpr-tag? x)
  (symbol? x))

(define (txexpr-attr? x)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))


(define (txexpr-attrs? x)
 (and (list? x) (andmap txexpr-attr? x)))

(define (txexpr-element? x)
  (xexpr? x))

(define (txexpr-elements? x)
 (and (list? x) (andmap txexpr-element? x)))

(define (txexpr-attr-key? x)
  (symbol? x))

(define (can-be-txexpr-attr-key? x)
  (or (symbol? x) (string? x)))

(define (txexpr-attr-value? x)
  (string? x))

(define (txexpr-attr-values? x)
  (and (list? x) (andmap txexpr-attr-value? x)))

(define (can-be-txexpr-attr-value? x)
  (or (symbol? x) (string? x)))

(define (can-be-txexpr-attrs? x) 
  (ormap (λ(test) (test x)) (list txexpr-attr? txexpr-attrs? can-be-txexpr-attr-key? can-be-txexpr-attr-value?)))

(define (list-of-can-be-txexpr-attrs? xs)
  (and (list? xs) (andmap can-be-txexpr-attrs? xs)))

