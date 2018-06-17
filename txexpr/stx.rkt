#lang racket/base

(provide stx-xexpr?
         stx-txexpr?
         stx-txexpr-tag?
         stx-txexpr-attrs?
         stx-txexpr->values
         stx-txexpr->list
         stx-txexpr-tag
         stx-txexpr-attrs
         stx-txexpr-elements)

(require syntax/stx
         xml
         "base.rkt"
         "private/container.rkt")

;; ---------------------------------------------------------

;; Data Definitions

;; A [Stx E] is one of:
;;  - (Syntaxof E)
;;  - E

;; A StxXexpr is a [Stx XexprE]
;; A StxTXexpr is a [Stx TXexprE]
;; A StxTag is a [Stx Symbol]
;; A StxAttrs is a (StxListof (StxList [Stx Symbol] [Stx String]))
;; A StxElements is a (StxListof StxXexpr)

;; A XexprE is one of:
;;  - String
;;  - TXexprE
;;  - Symbol       ; for example 'nbsp representing `&nbsp;`
;;  - ValidChar    ; for example #x20 representing `&#x20;`
;;  - CData        ; an instance of the `cdata` structure type from `xml`
;;  - Misc         ; an instance of the `comment` or `p-i` structure types

;; A TXexprE is one of:
;;  - (list* StxTag StxAttrs StxElements)
;;  - (list* StxTag StxElements)

;; The types `StxAttrs` and `StxXexpr` are disjoint, as they
;; need to be for this to be unambiguous.

;; ---------------------------------------------------------

;; Predicates

;; Any -> Bool
(define (stx-xexpr? v)
  (stx-xexpr?/recur v stx-xexpr?))

;; Any -> Bool
(define (stx-txexpr? v)
  (stx-txexpr?/recur v stx-xexpr?))

;; Any [Any -> Bool] -> Bool
(define (stx-xexpr?/recur v rec)
  (cond
    [(syntax? v) (stx-xexpr?/recur (syntax-e v) rec)]
    [(pair? v) (stx-txexpr?/recur v rec)]
    [else (xexpr?/recur v rec)]))

;; Any [Any -> Bool] -> Bool
;; the `rec` predicate should not overlap with `txexpr-attrs?`
(define (xexpr?/recur v rec)
  (cond
    [(pair? v) (txexpr?/recur v txexpr-tag? txexpr-attrs? rec)]
    [(string? v) #true]
    [(symbol? v) #true]
    [(integer? v) (valid-char? v)]
    [(cdata? v) #true]
    [(comment? v) #true]
    [(p-i? v) #true]
    [else #false]))

;; Any [Any -> Bool] -> Bool
;; the `rec` predicate should not overlap with
;; `stx-txexpr-attrs?`
(define (stx-txexpr?/recur v rec)
  ;; Even if it's not stx, the cdr or cddr might be syntax.
  ;; This flattens it so that the cdd...r is always a pair or empty.
  (define lst (stx->list v))
  (and lst (txexpr?/recur lst stx-txexpr-tag? stx-txexpr-attrs? rec)))

;; Any -> Bool
(define (stx-txexpr-tag? v)
  (cond
    [(syntax? v) (txexpr-tag? (syntax-e v))]
    [else (txexpr-tag? v)]))

;; Any -> Bool
(define (stx-txexpr-attrs? v)
  (txexpr-attrs? (syntax->datum (datum->syntax #f v))))

;; ---------------------------------------------------------

;; Accessors

;; StxTxexpr -> (values StxTag StxAttrs StxElements)
(define (stx-txexpr->values v)
  (txexpr->values/attrs? (stx->list v) stx-txexpr-attrs?))

;; StxTxexpr -> (List StxTag StxAttrs StxElements)
(define (stx-txexpr->list v)
  (define-values [tag attrs elements] (stx-txexpr->values v))
  (list tag attrs elements))

;; StxTxexpr -> StxTag
(define (stx-txexpr-tag v)
  (define-values [tag attrs elements] (stx-txexpr->values v))
  tag)

;; StxTxexpr -> StxAttrs
(define (stx-txexpr-attrs v)
  (define-values [tag attrs elements] (stx-txexpr->values v))
  attrs)

;; StxTxexpr -> StxElements
(define (stx-txexpr-elements v)
  (define-values [tag attrs elements] (stx-txexpr->values v))
  elements)

;; ---------------------------------------------------------

