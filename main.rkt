#lang racket/base
(require racket/contract racket/match xml racket/list)
(require sugar)


;; a tagged-xexpr consists of a tag, optional attributes, and then elements.

(define+provide/contract (xexpr-tag? x)
  (any/c . -> . boolean?)
  (symbol? x)) 

(define+provide/contract (xexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    [(list (list (? symbol?) (? string?)) ...) #t]
    [else #f]))

(define+provide/contract (xexpr-element? x)
  (any/c . -> . boolean?)
  (or (string? x) (tagged-xexpr? x) (symbol? x)
      (valid-char? x) (cdata? x)))

(define+provide/contract (xexpr-elements? x)
  (any/c . -> . boolean?)
  (match x
    [(list elem ...) (andmap xexpr-element? elem)]
    [else #f]))

;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define+provide/contract (tagged-xexpr? x)
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
(define+provide/contract (make-xexpr-attr . items)
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
(define+provide/contract (make-tagged-xexpr name [attr empty] [content empty])
  ; xexpr/c provides a nicer error message,
  ; but is not sufficient on its own (too permissive)
  ((symbol?) (xexpr-attr? (listof xexpr-element?)) 
             . ->* . tagged-xexpr?)
  (filter-not empty? `(,name ,attr ,@content)))



;; decompose tagged-xexpr into parts (opposite of make-tagged-xexpr)
(define+provide/contract (break-tagged-xexpr x)
  (tagged-xexpr? . -> . 
                 (values symbol? xexpr-attr? (listof xexpr-element?)))
  (match 
      ; tagged-xexpr may or may not have attr
      ; if not, add empty attr so that decomposition only handles one case
      (match x
        [(list _ (? xexpr-attr?) _ ...) x]
        [else `(,(car x) ,empty ,@(cdr x))])
    [(list tag attr content ...) (values tag attr content)]))



;; convenience functions to retrieve only one part of tagged-xexpr
(define+provide/contract (tagged-xexpr-tag x)
  (tagged-xexpr? . -> . xexpr-tag?)
  (car x))

(define+provide/contract (tagged-xexpr-attr x)
  (tagged-xexpr? . -> . xexpr-attr?)
  (define-values (tag attr content) (break-tagged-xexpr x))
  attr)

(define+provide/contract (tagged-xexpr-elements x)
  (tagged-xexpr? . -> . (listof xexpr-element?))
  (define-values (tag attrt elements) (break-tagged-xexpr x))
  elements)


;; remove all attr blocks (helper function)
(define+provide/contract (remove-attrs x)
  (tagged-xexpr? . -> . tagged-xexpr?)
  (match x
    [(? tagged-xexpr?) (let-values ([(tag attr elements) (break-tagged-xexpr x)])
                         (make-tagged-xexpr tag empty (remove-attrs elements)))]
    [(? list?) (map remove-attrs x)]
    [else x]))


(define+provide/contract (map-xexpr-elements proc x)
  (procedure? tagged-xexpr? . -> . tagged-xexpr?)
  (define-values (tag attr elements) (break-tagged-xexpr x)) 
  (make-tagged-xexpr tag attr (map proc elements)))



;; function to split tag out of tagged-xexpr
(define+provide/contract (split-tag-from-xexpr tag tx)
  (xexpr-tag? tagged-xexpr? . -> . (values (listof xexpr-element?) tagged-xexpr? ))
  (define matches '())
  (define (extract-tag x)
    (cond
      [(and (tagged-xexpr? x) (equal? tag (car x)))
       ; stash matched tag but return empty value
       (begin
         (set! matches (cons x matches))
         empty)]
      [(tagged-xexpr? x) (let-values([(tag attr body) (break-tagged-xexpr x)]) 
                           (make-tagged-xexpr tag attr (extract-tag body)))]
      [(xexpr-elements? x) (filter-not empty? (map extract-tag x))]
      [else x]))
  (define tx-extracted (extract-tag tx)) ;; do this first to fill matches
  (values (reverse matches) tx-extracted)) 