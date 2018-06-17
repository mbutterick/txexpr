#lang racket/base

(provide txexpr?/recur
         txexpr->values/attrs?)

(require racket/list)

;; A [TXcontainer T A E] is one of:
;;  - (List* T A (Listof E))
;;  - (Lxst* T (Listof E))
;; Where A and E are disjoint.

;; txexpr?/recur :
;;   Any
;;   [Any -> Bool : T]
;;   [Any -> Bool : #:+ (and A (! E)) #:- (! A)]
;;   [Any -> Bool : #:+ (and E (! A)) #:- (! E)]
;;   ->
;;   Bool
;;   : [TXcontainer T A E]
;; the attrs? predicate and the element? predicate should be disjoint
(define (txexpr?/recur v tag? attrs? element?)
  (and (list? v)
       (not (empty? v))
       (tag? (first v))
       (cond [(and (not (empty? (rest v)))
                   (attrs? (second v)))
              (andmap element? (rest (rest v)))]
             [else
              (andmap element? (rest v))])))

;; txexpr->values/attrs? :
;;   [TXcontainer T A E]
;;   [Any -> Bool : #:+ (and A (! E)) #:- (! A)]
;;   ->
;;   (values T A (Listof E))
(define (txexpr->values/attrs? tx attrs?)
  (cond [(and (not (empty? (rest tx)))
              (attrs? (second tx)))
         (values (first tx) (second tx) (rest (rest tx)))]
        [else
         (values (first tx) '() (rest tx))]))

