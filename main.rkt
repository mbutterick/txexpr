#lang racket/base
(require (for-syntax racket/base))
(require racket/contract racket/match xml racket/list)

(define-syntax (define+provide/contract stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide/contract proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (provide (contract-out [name contract]))
         (define name body ...))]))

;; a tagged-xexpr consists of a tag, optional attributes, and then elements.

(define+provide/contract (tagged-xexpr-tag? x)
  (any/c . -> . boolean?)
  (symbol? x)) 


(define+provide/contract (tagged-xexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))

(define+provide/contract (tagged-xexpr-attrs? x)
  (any/c . -> . boolean?)
  (match x
    [(list (? tagged-xexpr-attr?) ...) #t]
    [else #f]))

(define+provide/contract (tagged-xexpr-element? x)
  (any/c . -> . boolean?)
  (or (string? x) (tagged-xexpr? x) (symbol? x)
      (valid-char? x) (cdata? x)))

(define+provide/contract (tagged-xexpr-elements? x)
  (any/c . -> . boolean?)
  (match x
    [(list elem ...) (andmap tagged-xexpr-element? elem)]
    [else #f]))

;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define+provide/contract (tagged-xexpr? x)
  (any/c . -> . boolean?)
  (and (xexpr? x) ; meets basic xexpr contract
       (match x
         ;; is a list starting with a symbol
         [(list (? symbol? name) rest ...) 
          ;; the rest is content or ...
          (or (andmap tagged-xexpr-element? rest) 
              ;; attr + content 
              (and (tagged-xexpr-attrs? (car rest)) (andmap tagged-xexpr-element? (cdr rest))))] 
         [else #f])))


(define+provide/contract (make-tagged-xexpr tag [attrs empty] [elements empty])
  ;; todo?: use xexpr/c provides a nicer error message
  ((symbol?) (tagged-xexpr-attrs? (listof tagged-xexpr-element?)) 
             . ->* . tagged-xexpr?)
  (filter-not empty? `(,tag ,attrs ,@elements)))


(define+provide/contract (tagged-xexpr->values x)
  (tagged-xexpr? . -> . 
                 (values symbol? tagged-xexpr-attrs? (listof tagged-xexpr-element?)))
  (match 
      ; tagged-xexpr may or may not have attr
      ; if not, add empty attr so that decomposition only handles one case
      (match x
        [(list _ (? tagged-xexpr-attrs?) _ ...) x]
        [else `(,(car x) ,empty ,@(cdr x))])
    [(list tag attr content ...) (values tag attr content)]))


(define+provide/contract (tagged-xexpr->list x)
  (tagged-xexpr? . -> . list?)
  (define-values (tag attrs content) (tagged-xexpr->values x))
  (list tag attrs content))


;; convenience functions to retrieve only one part of tagged-xexpr
(define+provide/contract (tagged-xexpr-tag x)
  (tagged-xexpr? . -> . tagged-xexpr-tag?)
  (car x))


(define+provide/contract (tagged-xexpr-attrs x)
  (tagged-xexpr? . -> . tagged-xexpr-attrs?)
  (define-values (tag attrs content) (tagged-xexpr->values x))
  attrs)


(define+provide/contract (tagged-xexpr-elements x)
  (tagged-xexpr? . -> . (listof tagged-xexpr-element?))
  (define-values (tag attrs elements) (tagged-xexpr->values x))
  elements)


;; helpers. we are getting a string or symbol
(define (->symbol x)
  (if (string? x) (string->symbol x) x))

(define (->string x)
  (if (symbol? x) (symbol->string x) x))


;; convert list of alternating keys & values to attr
(define+provide/contract (merge-attrs . items)
  (() #:rest (listof (or/c tagged-xexpr-attr? tagged-xexpr-attrs? symbol? string?)) . ->* . tagged-xexpr-attrs?)
  
  ;; need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define (make-attr-list items)
    (if (empty? items)
        empty
        (let ([key (->symbol (first items))]
              [value (->string (second items))]
              [rest (drop items 2)])
          (append (list key value) (make-attr-list rest)))))
  
  ;; use hash to ensure keys are unique (later values will overwrite earlier)
  (define attr-hash (apply hash (make-attr-list (flatten items))))
  `(,@(map (λ(k) (list k (hash-ref attr-hash k))) 
           ;; sort needed for predictable results for unit tests
           (sort (hash-keys attr-hash) (λ(a b) (string<? (->string a) (->string b)))))))


(define+provide/contract (remove-attrs x)
  (tagged-xexpr? . -> . tagged-xexpr?)
  (match x
    [(? tagged-xexpr?) (let-values ([(tag attr elements) (tagged-xexpr->values x)])
                         (make-tagged-xexpr tag empty (remove-attrs elements)))]
    [(? list?) (map remove-attrs x)]
    [else x]))


(define+provide/contract (map-elements proc x)
  (procedure? tagged-xexpr? . -> . tagged-xexpr?)
  (define-values (tag attr elements) (tagged-xexpr->values x)) 
  (define recursive-proc
    (λ(x) 
      (cond
        [(tagged-xexpr? x) (map-elements proc x)]
        [else (proc x)])))
  (make-tagged-xexpr tag attr (map recursive-proc elements)))


