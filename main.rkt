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

(define+provide/contract (txexpr-tag? x)
  (any/c . -> . boolean?)
  (symbol? x)) 

(define+provide/contract (txexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))

(define+provide/contract (txexpr-attrs? x)
  (any/c . -> . boolean?)
  (match x
    [(list (? txexpr-attr?) ...) #t]
    [else #f]))

(define+provide/contract (txexpr-element? x)
  (any/c . -> . boolean?)
  (or (string? x) (txexpr? x) (symbol? x)
      (valid-char? x) (cdata? x)))

(define+provide/contract (txexpr-elements? x)
  (any/c . -> . boolean?)
  (match x
    [(list elem ...) (andmap txexpr-element? elem)]
    [else #f]))

;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define+provide/contract (txexpr? x)
  (any/c . -> . boolean?)
  (and (xexpr? x) ; meets basic xexpr contract
       (match x
         [(list (? symbol? name) rest ...)          ;; is a list starting with a symbol
          (or (empty? rest) 
              (andmap txexpr-element? rest)           ;; the rest is content or ...
              (and (txexpr-attrs? (car rest)) (andmap txexpr-element? (cdr rest))))] ;; attr + content 
         [else #f])))

(define+provide/contract (make-txexpr tag [attrs empty] [elements empty])
  ;; todo?: use xexpr/c provides a nicer error message
  ((symbol?) (txexpr-attrs? (listof txexpr-element?)) 
             . ->* . txexpr?)
  (filter-not empty? `(,tag ,attrs ,@elements)))


(define+provide/contract (txexpr->values x)
  (txexpr? . -> . 
           (values symbol? txexpr-attrs? (listof txexpr-element?)))
  (match 
      ; txexpr may or may not have attr
      ; if not, add empty attr so that decomposition only handles one case
      (match x
        [(list _ (? txexpr-attrs?) _ ...) x]
        [else `(,(car x) ,empty ,@(cdr x))])
    [(list tag attr content ...) (values tag attr content)]))


(define+provide/contract (txexpr->list x)
  (txexpr? . -> . list?)
  (define-values (tag attrs content) (txexpr->values x))
  (list tag attrs content))


;; convenience functions to retrieve only one part of txexpr
(define+provide/contract (get-tag x)
  (txexpr? . -> . txexpr-tag?)
  (car x))


(define+provide/contract (get-attrs x)
  (txexpr? . -> . txexpr-attrs?)
  (define-values (tag attrs content) (txexpr->values x))
  attrs)


(define+provide/contract (get-elements x)
  (txexpr? . -> . (listof txexpr-element?))
  (define-values (tag attrs elements) (txexpr->values x))
  elements)


;; helpers. we are getting a string or symbol
(define+provide/contract (->txexpr-attr-key x)
  (can-be-txexpr-attr-key? . -> . txexpr-attr-key?)
  (if (string? x) (string->symbol x) x))

(define+provide/contract (->txexpr-attr-value x)
  (can-be-txexpr-attr-value? . -> . txexpr-attr-value?)
  (->string x))

(define+provide/contract (txexpr-attr-key? x) 
  (any/c . -> . boolean?)
  (symbol? x))

(define+provide/contract (can-be-txexpr-attr-key? x)
  (any/c . -> . boolean?)
  (or (symbol? x) (string? x)))

(define+provide/contract (txexpr-attr-value? x) 
  (any/c . -> . boolean?)
  (string? x))

(define+provide/contract (can-be-txexpr-attr-value? x) 
  (any/c . -> . boolean?)
  (can-be-txexpr-attr-key? x))

(define (->string x) 
  (any/c . -> . string?)
  (if (symbol? x) (symbol->string x) x))

(define+provide/contract (can-be-txexpr-attrs? x) 
  (any/c . -> . boolean?)
  ((or/c txexpr-attr? txexpr-attrs? can-be-txexpr-attr-key? can-be-txexpr-attr-value?) x))

(define+provide/contract (attrs->hash . items)
  (() #:rest (listof can-be-txexpr-attrs?) . ->* . hash?)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define (make-key-value-list items)
    (if (empty? items)
        empty
        (let ([key (->txexpr-attr-key (first items))]
              [value (->txexpr-attr-value (second items))]
              [rest (drop items 2)])
          (cons (cons key value) (make-key-value-list rest)))))
  (make-immutable-hash (make-key-value-list (flatten items))))

(define+provide/contract (hash->attrs hash)
  (hash? . -> . txexpr-attrs?)
  (hash-map hash list))

(define+provide/contract (attr-set tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (define new-attrs 
    (hash->attrs (hash-set (attrs->hash (get-attrs tx)) (->txexpr-attr-key key) (->txexpr-attr-value value))))
  (make-txexpr (get-tag tx) new-attrs (get-elements tx)))


(define+provide/contract (attr-ref tx key)
  (txexpr? can-be-txexpr-attr-key? . -> . txexpr-attr-value?)
  (with-handlers ([exn:fail? (λ(e) (error (format "attr-ref: no value found for key ~v" key)))])
    (hash-ref (attrs->hash (get-attrs tx)) key)))

;; convert list of alternating keys & values to attr
(define+provide/contract (merge-attrs . items)
  (() #:rest (listof can-be-txexpr-attrs?) . ->* . txexpr-attrs?)
  (define attrs-hash (apply attrs->hash items))
  ;; sort needed for predictable results for unit tests
  (define sorted-hash-keys (sort (hash-keys attrs-hash) (λ(a b) (string<? (->string a) (->string b)))))
  `(,@(map (λ(key) (list key (hash-ref attrs-hash key))) sorted-hash-keys)))


(define+provide/contract (remove-attrs x)
  (txexpr? . -> . txexpr?)
  (cond
    [(txexpr? x) (let-values ([(tag attr elements) (txexpr->values x)])
                   (make-txexpr tag empty (remove-attrs elements)))]
    [(txexpr-elements? x) (map remove-attrs x)]
    [else x]))


;; todo: exclude-proc will keep things out, but is there a way to keep things in?
(define+provide/contract (map-elements/exclude proc x exclude-test)
  (procedure? txexpr? procedure? . -> . txexpr?)
  (cond
    [(txexpr? x) 
     (if (exclude-test x)
         x
         (let-values ([(tag attr elements) (txexpr->values x)])
           (make-txexpr tag attr 
                        (map (λ(x)(map-elements/exclude proc x exclude-test)) elements))))]
    ;; externally the function only accepts txexpr,
    ;; but internally we don't care
    [else (proc x)]))

(define+provide/contract (map-elements proc x)
  (procedure? txexpr? . -> . txexpr?)
  (map-elements/exclude proc x (λ(x) #f)))

;; function to split tag out of txexpr
(define+provide/contract (splitf-txexpr tx proc)
  ((and/c xexpr/c txexpr?) procedure? . -> . (values txexpr? (listof txexpr-element?)))
  (define matches empty)
  (define (do-extraction x)
    (cond
      [(proc x) (begin  ; store matched item but return empty value
                  (set! matches (cons x matches))
                  empty)]
      [(txexpr? x) (let-values([(tag attr body) (txexpr->values x)]) 
                     (make-txexpr tag attr (do-extraction body)))]
      [(txexpr-elements? x) (filter-not empty? (map do-extraction x))]
      [else x]))
  (define tx-extracted (do-extraction tx)) ;; do this first to fill matches
  (values tx-extracted (reverse matches))) 

