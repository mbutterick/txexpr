#lang racket/base
(require (for-syntax racket/base))
(require racket/match xml)

(module+ safe (require racket/contract))

(define-syntax (define+provide+safe stx)
  (syntax-case stx ()
    [(_ (proc arg ... . rest-arg) contract body ...)
     #'(define+provide+safe proc contract
         (λ(arg ... . rest-arg) body ...))]
    [(_ name contract body ...)
     #'(begin
         (define name body ...)
         (provide name)
         (module+ safe 
           (provide (contract-out [name contract]))))]))

(define+provide+safe (txexpr-tag? x)
  (any/c . -> . boolean?)
  (symbol? x)) 

(define+provide+safe (txexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))


(require racket/string)
(define (validate-txexpr-attrs? x #:context [txexpr-context #f])
  
  (define (make-reason) 
    (if (not (list? x)) 
        (format "because ~v is not a list" x)
        (let ([bad-attrs (filter (λ(i) (not (txexpr-attr? i))) x)])
          (format "because ~a ~a" (string-join (map (λ(ba) (format "~v" ba)) bad-attrs) " and ") (if (> (length bad-attrs) 1)
                                                "are not valid txexpr-attrs"
                                                "is not a valid attr")))))
  
  (match x
    [(list (? txexpr-attr?) ...) #t]
    [else [else (error (string-append "validate-txexpr-attrs: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid list of attrs ~a" x (make-reason))))]]))

(define+provide+safe (txexpr-attrs? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (validate-txexpr-attrs? x)))


(define+provide+safe (txexpr-elements? x)
  (any/c . -> . boolean?)
  (match x
    [(list elem ...) (andmap txexpr-element? elem)]
    [else #f]))

(define (validate-txexpr-element? x #:context [txexpr-context #f])
  (cond
    [(or (string? x) (txexpr? x) (symbol? x)
         (valid-char? x) (cdata? x)) #t]
    [else (error (string-append "validate-txexpr-element: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid element (expecting txexpr, string, symbol, XML char, or cdata)" x)))]))


(define+provide+safe (txexpr-element? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (validate-txexpr-element? x)))

;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define+provide+safe (validate-txexpr? x)
  (any/c . -> . boolean?)
  (define (validate-txexpr-element-with-context? e) (validate-txexpr-element? e #:context x))
  (define (validate-txexpr-attrs-with-context? e) (validate-txexpr-attrs? e #:context x))
  
  (match x
    [(list (? symbol? name) rest ...)          ;; is a list starting with a symbol
     (or (null? rest) 
         (andmap txexpr-element? rest)           ;; the rest is content or ...
         (and (validate-txexpr-attrs-with-context? (car rest)) 
              (andmap validate-txexpr-element-with-context? (cdr rest))))] ;; attr + content 
    [else (error (format "validate-txexpr: first element is not a symbol in ~v" x))]))

(define+provide+safe (txexpr? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (validate-txexpr? x)))



(define+provide+safe (make-txexpr tag [attrs null] [elements null])
  ;; todo?: use xexpr/c provides a nicer error message
  ((symbol?) (txexpr-attrs? (listof txexpr-element?)) 
             . ->* . txexpr?)
  (filter (compose1 not null?) `(,tag ,attrs ,@elements)))


(define+provide+safe (txexpr->values x)
  (txexpr? . -> . 
           (values symbol? txexpr-attrs? (listof txexpr-element?)))
  (match 
      ; txexpr may or may not have attr
      ; if not, add null attr so that decomposition only handles one case
      (match x
        [(list _ (? txexpr-attrs?) _ ...) x]
        [else `(,(car x) ,null ,@(cdr x))])
    [(list tag attr content ...) (values tag attr content)]))


(define+provide+safe (txexpr->list x)
  (txexpr? . -> . list?)
  (define-values (tag attrs content) (txexpr->values x))
  (list tag attrs content))


;; convenience functions to retrieve only one part of txexpr
(define+provide+safe (get-tag x)
  (txexpr? . -> . txexpr-tag?)
  (car x))


(define+provide+safe (get-attrs x)
  (txexpr? . -> . txexpr-attrs?)
  (define-values (tag attrs content) (txexpr->values x))
  attrs)


(define+provide+safe (get-elements x)
  (txexpr? . -> . (listof txexpr-element?))
  (define-values (tag attrs elements) (txexpr->values x))
  elements)


;; helpers. we are getting a string or symbol
(define+provide+safe (->txexpr-attr-key x)
  (can-be-txexpr-attr-key? . -> . txexpr-attr-key?)
  (if (string? x) (string->symbol x) x))

(define+provide+safe (->txexpr-attr-value x)
  (can-be-txexpr-attr-value? . -> . txexpr-attr-value?)
  (->string x))

(define+provide+safe (txexpr-attr-key? x) 
  (any/c . -> . boolean?)
  (symbol? x))

(define+provide+safe (can-be-txexpr-attr-key? x)
  (any/c . -> . boolean?)
  (or (symbol? x) (string? x)))

(define+provide+safe (txexpr-attr-value? x) 
  (any/c . -> . boolean?)
  (string? x))

(define+provide+safe (can-be-txexpr-attr-value? x) 
  (any/c . -> . boolean?)
  (can-be-txexpr-attr-key? x))

(define (->string x) 
  (if (symbol? x) (symbol->string x) x))

(define+provide+safe (can-be-txexpr-attrs? x) 
  (any/c . -> . boolean?)
  (ormap (λ(test) (test x)) (list txexpr-attr? txexpr-attrs? can-be-txexpr-attr-key? can-be-txexpr-attr-value?)))

(define (flatten orig-sexp)
  (let loop ([sexp orig-sexp] [acc null])
    (cond [(null? sexp) acc]
          [(pair? sexp) (loop (car sexp) (loop (cdr sexp) acc))]
          [else (cons sexp acc)])))

(define+provide+safe (attrs->hash . items)
  (() #:rest (listof can-be-txexpr-attrs?) . ->* . hash?)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define (make-key-value-list items)
    (if (null? items)
        null
        (let ([key (->txexpr-attr-key (car items))]
              [value (->txexpr-attr-value (cadr items))]
              [rest (cddr items)])
          (cons (cons key value) (make-key-value-list rest)))))
  (make-immutable-hash (make-key-value-list (flatten items))))

(define+provide+safe (hash->attrs hash)
  (hash? . -> . txexpr-attrs?)
  (hash-map hash list))

(define+provide+safe (attrs-have-key? x key)
  ((or/c txexpr-attrs? txexpr?) can-be-txexpr-attr-key? . -> . boolean?)
  (define attrs (if (txexpr-attrs? x) x (get-attrs x)))
  (hash-has-key? (attrs->hash attrs) (->txexpr-attr-key key)))

(define+provide+safe (attr-set tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (define new-attrs 
    (hash->attrs (hash-set (attrs->hash (get-attrs tx)) (->txexpr-attr-key key) (->txexpr-attr-value value))))
  (make-txexpr (get-tag tx) new-attrs (get-elements tx)))


(define+provide+safe (attr-ref tx key)
  (txexpr? can-be-txexpr-attr-key? . -> . txexpr-attr-value?)
  (with-handlers ([exn:fail? (λ(e) (error (format "attr-ref: no value found for key ~v" key)))])
    (hash-ref (attrs->hash (get-attrs tx)) key)))

;; convert list of alternating keys & values to attr
(define+provide+safe (merge-attrs . items)
  (() #:rest (listof can-be-txexpr-attrs?) . ->* . txexpr-attrs?)
  (define attrs-hash (apply attrs->hash items))
  ;; sort needed for predictable results for unit tests
  (define sorted-hash-keys (sort (hash-keys attrs-hash) (λ(a b) (string<? (->string a) (->string b)))))
  `(,@(map (λ(key) (list key (hash-ref attrs-hash key))) sorted-hash-keys)))


(define+provide+safe (remove-attrs x)
  (txexpr? . -> . txexpr?)
  (cond
    [(txexpr? x) (let-values ([(tag attr elements) (txexpr->values x)])
                   (make-txexpr tag null (remove-attrs elements)))]
    [(txexpr-elements? x) (map remove-attrs x)]
    [else x]))


;; todo: exclude-proc will keep things out, but is there a way to keep things in?
(define+provide+safe (map-elements/exclude proc x exclude-test)
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

(define+provide+safe (map-elements proc x)
  (procedure? txexpr? . -> . txexpr?)
  (map-elements/exclude proc x (λ(x) #f)))

;; function to split tag out of txexpr
(define+provide+safe (splitf-txexpr tx proc)
  (txexpr? procedure? . -> . (values txexpr? (listof txexpr-element?)))
  (define matches null)
  (define (do-extraction x)
    (cond
      [(proc x) (begin  ; store matched item but return null value
                  (set! matches (cons x matches))
                  null)]
      [(txexpr? x) (let-values([(tag attr body) (txexpr->values x)]) 
                     (make-txexpr tag attr (do-extraction body)))]
      [(txexpr-elements? x) (filter (compose1 not null?) (map do-extraction x))]
      [else x]))
  (define tx-extracted (do-extraction tx)) ;; do this first to fill matches
  (values tx-extracted (reverse matches))) 


(define+provide+safe (txexpr->html x)
  (txexpr? . -> . string?)
  (define (->cdata x)
    (if (cdata? x) x (cdata #f #f x)))
  
  (xexpr->string (let loop ([x x])
                   (cond
                     [(txexpr? x) (if (member (get-tag x) '(script style))
                                      (make-txexpr (get-tag x) (get-attrs x) (map ->cdata (get-elements x)))
                                      (make-txexpr (get-tag x) (get-attrs x) (map loop (get-elements x))))]
                     [else x]))))

