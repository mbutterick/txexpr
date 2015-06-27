#lang racket/base
(require sugar/define xml racket/match)
(provide (all-defined-out) cdata? cdata valid-char? xexpr->string xexpr?)

;; Section 2.2 of XML 1.1
;; (XML 1.0 is slightly different and more restrictive)
;; make private version of my-valid-char to get consistent results with 6.0
(define (my-valid-char? i)
  (and (exact-nonnegative-integer? i)
       (or (<= #x1     i #xD7FF)
           (<= #xE000  i #xFFFD)
           (<= #x10000 i #x10FFFF))))

(define (my-xexpr? x)
  (or (txexpr? x) (xexpr? x) (my-valid-char? x)))

(define (txexpr-short? x)
  (match x
    [(list (? symbol? name) (? my-xexpr?) ...) #t]
    [else #f]))

(define (txexpr? x)
  (or (txexpr-short? x)
      (match x
        [(list (? symbol?) (list (list (? symbol?) (? string?)) ...) (? my-xexpr?) ...) #t]
        [else #f])))

(define (txexpr-tag? x)
  (symbol? x))

(define (txexpr-tags? x)
  (and (list? x) (andmap txexpr-tag? x)))

(define (txexpr-attr? x)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))


(define (txexpr-attrs? x)
 (and (list? x) (andmap txexpr-attr? x)))

(define (txexpr-element? x)
  (my-xexpr? x))

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

