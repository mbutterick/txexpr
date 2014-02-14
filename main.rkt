#lang racket/base
(require racket/contract racket/match xml racket/list)
(require sugar)


(provide (all-defined-out))

;; is it an xexpr tag?
(define/contract (xexpr-tag? x)
  (any/c . -> . boolean?)
  (symbol? x)) 

;; is it an xexpr attributes?
(define/contract (xexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    ; list of symbol + string pairs
    [(list (list (? symbol? key) (? string? value)) ...) #t]
    [else #f]))



;; is it xexpr content?
(define/contract (xexpr-element? x)
  (any/c . -> . boolean?)
  (or (string? x) (tagged-xexpr? x)))

;; Not a great idea to use "plural" (i.e. listlike) contracts.
;; Instead of foobars? use (listof foobar?) as contract
;; Reason is that listof will show you the specific element that fails
;; whereas foobars? will just announce the result for the whole list.
;; Since contracts are intended to tell you why your input is defective,
;; the (listof foobar?) behavior is better.
;; outside of contracts, instead of testing (foobars? list),
;; test (andmap foobar? list)

(define/contract (xexpr-elements? x)
  (any/c . -> . boolean?)
  (match x
    ;; this is more strict than xexpr definition in xml module
    ;; don't allow symbols or numbers to be part of content
    [(list elem ...) (andmap xexpr-element? elem)]
    [else #f]))


;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define/contract (tagged-xexpr? x)
  (any/c . -> . boolean?)
  (and (xexpr? x) ; meets basic xexpr contract
       (match x
         [(list (? symbol? name) rest ...) ; is a list starting with a symbol
          (or (andmap xexpr-element? rest) ; the rest is content or ...
              (and (xexpr-attr? (car rest)) (andmap xexpr-element? (cdr rest))))] ; attr + content 
         [else #f])))



;; convert list of alternating keys & values to attr
;; todo: make contract. Which is somewhat complicated:
;; list of items, made of xexpr-attr or even numbers of symbol/string pairs
;; use splitf*-at with xexpr-attr? as test, then check lengths of resulting lists
(define/contract (make-xexpr-attr . items)
  (() #:rest (listof (λ(i) (or (xexpr-attr? i) (symbol? i) (string? i)))) . ->* . xexpr-attr?)
  
  ;; need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define (make-attr-list items)
    (if (empty? items)
        empty
        (let ([key (->symbol (first items))]
              [value (->string (second items))]
              [rest (drop items 2)])
          (append (list key value) (make-attr-list rest)))))
  
  ;; use flatten to splice xexpr-attrs into list
  ;; use hash to ensure keys are unique (later values will overwrite earlier)
  (define attr-hash (apply hash (make-attr-list (flatten items))))
  `(,@(map (λ(k) (list k (get attr-hash k))) 
           ;; sort needed for predictable results for unit tests
           (sort (hash-keys attr-hash) (λ(a b) (string<? (->string a) (->string b)))))))






;; create tagged-xexpr from parts (opposite of break-tagged-xexpr)
(define/contract (make-tagged-xexpr name [attr empty] [content empty])
  ; xexpr/c provides a nicer error message,
  ; but is not sufficient on its own (too permissive)
  ((symbol?) (xexpr-attr? (listof xexpr-element?)) 
             . ->* . tagged-xexpr?)
  (filter-not empty? `(,name ,attr ,@content)))



;; decompose tagged-xexpr into parts (opposite of make-tagged-xexpr)
(define/contract (break-tagged-xexpr nx)
  (tagged-xexpr? . -> . 
                 (values symbol? xexpr-attr? (listof xexpr-element?)))
  (match 
      ; tagged-xexpr may or may not have attr
      ; if not, add empty attr so that decomposition only handles one case
      (match nx
        [(list _ (? xexpr-attr?) _ ...) nx]
        [else `(,(car nx) ,empty ,@(cdr nx))])
    [(list tag attr content ...) (values tag attr content)]))



;; convenience functions to retrieve only one part of tagged-xexpr
(define (tagged-xexpr-tag nx)
  (tagged-xexpr? . -> . xexpr-tag?)
  (define-values (tag attr content) (break-tagged-xexpr nx))
  tag)

(define (tagged-xexpr-attr nx)
  (tagged-xexpr? . -> . xexpr-attr?)
  (define-values (tag attr content) (break-tagged-xexpr nx))
  attr)

(define (tagged-xexpr-elements nx)
  (tagged-xexpr? . -> . (listof xexpr-element?))
  (define-values (tag attrt elements) (break-tagged-xexpr nx))
  elements)




;; remove all attr blocks (helper function)
(define/contract (remove-attrs x)
  (tagged-xexpr? . -> . tagged-xexpr?)
  (match x
    [(? tagged-xexpr?) (let-values ([(tag attr elements) (break-tagged-xexpr x)])
                         (make-tagged-xexpr tag empty (remove-attrs elements)))]
    [(? list?) (map remove-attrs x)]
    [else x]))

