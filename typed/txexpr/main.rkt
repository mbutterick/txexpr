#lang typed/racket/base
(require (for-syntax typed/racket/base))
(require racket/match racket/string racket/list racket/bool "core-predicates.rkt")
(provide (all-defined-out) (all-from-out "core-predicates.rkt"))


(define/typed (validate-txexpr-attrs x #:context [txexpr-context #f])
  ((Any) (#:context Boolean) . ->* . Txexpr-Attrs)
  (define/typed (make-reason)
    (-> String)
    (if (not (list? x)) 
        (format "because ~v is not a list" x)
        (let ([bad-attrs (filter (λ(i) (not (txexpr-attr? i))) x)])
          (format "because ~a ~a" (string-join (map (λ(ba) (format "~v" ba)) bad-attrs) " and ") (if (> (length bad-attrs) 1)
                                                                                                     "are not valid attributes"
                                                                                                     "is not in the form '(symbol \"string\")")))))
  (cond
    [(and (list? x) (> 0 (length x)) (andmap txexpr-attr? x)) x]
    [else (error (string-append "validate-txexpr-attrs: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid list of attributes ~a" x (make-reason))))]))


(define/typed (validate-txexpr-element x #:context [txexpr-context #f])
  ((Any) (#:context Any) . ->* . Txexpr-Element)
  (cond
    [(or (string? x) (txexpr? x) (symbol? x)
         (valid-char? x) (cdata? x)) (cast x Txexpr-Element)]
    [else (error (string-append "validate-txexpr-element: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid element (must be txexpr, string, symbol, XML char, or cdata)" x)))]))


;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define/typed (validate-txexpr x)
  (Any -> Txexpr)
  (define-syntax-rule (validate-txexpr-element-with-context e) (validate-txexpr-element e #:context x))
  (define-syntax-rule (validate-txexpr-attrs-with-context e) (validate-txexpr-attrs e #:context x))
  (if (match x
        [(list (? symbol?)) #t]
        ;; todo: fix this condition
        #;[(list (? symbol?) (and attr-list (list (list k v ...) ...)) rest ...) 
           (and (validate-txexpr-attrs-with-context (cast attr-list Txexpr-Attrs)) 
                (andmap (λ(e) (validate-txexpr-element-with-context e)) rest))]
        [(list (? symbol? name) rest ...)(andmap (λ(e) (validate-txexpr-element-with-context e)) rest)]
        [else (error 'validate-txexpr (format "~v is not a list starting with a symbol" x))])
      (cast x Txexpr)
      (error 'validate-txexpr "Can't reach this")))


(define/typed (make-txexpr tag [attrs null] [elements null])
  ((Symbol) (Txexpr-Attrs (Listof Txexpr-Element)) . ->* . Txexpr)
  (cast (cons tag (append (if (empty? attrs) empty (list attrs)) elements)) Txexpr))


(define/typed (txexpr->values x)
  (Txexpr -> (values Txexpr-Tag Txexpr-Attrs Txexpr-Elements))
  (match 
      ; txexpr may or may not have attr
      ; if not, add null attr so that decomposition only handles one case
      (match x
        [(list _ (? txexpr-attrs?) _ ...) x]
        [else `(,(car x) ,null ,@(cdr x))])
    [(list tag attr content ...) (values tag (cast attr Txexpr-Attrs) (cast content Txexpr-Elements))]))


(define/typed (txexpr->list x)
  (Txexpr -> (List Txexpr-Tag Txexpr-Attrs Txexpr-Elements))
  (define-values (tag attrs content) (txexpr->values x))
  (list tag attrs content))


;; convenience functions to retrieve only one part of txexpr
(define/typed (get-tag x)
  (Txexpr -> Txexpr-Tag)
  (car x))


(define/typed (get-attrs x)
  (Txexpr -> Txexpr-Attrs)
  (define-values (tag attrs content) (txexpr->values x))
  attrs)


(define/typed (get-elements x)
  (Txexpr -> Txexpr-Elements)
  (define-values (tag attrs elements) (txexpr->values x))
  elements)


;; helpers. we are getting a string or symbol
(define/typed (->txexpr-attr-key x)
  (Can-Be-Txexpr-Attr-Key -> Txexpr-Attr-Key)
  (if (string? x) (string->symbol x) x))


(define/typed (->txexpr-attr-value x)
  (Can-Be-Txexpr-Attr-Value -> Txexpr-Attr-Value)
  (->string x))


(define/typed (->string x)
  ((U Symbol String) -> String)
  (if (symbol? x) (symbol->string x) x))


(define/typed (attrs->hash . items-in)
  (Can-Be-Txexpr-Attr * -> Txexpr-Attr-Hash)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define items (reverse
                 (for/fold ([items : (Listof (U Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value)) null])
                           ([i (in-list items-in)])
                   (cond
                     [(txexpr-attr? i) (append i items)]
                     [(txexpr-attrs? i) (append (append* i) items)]
                     [else (cons i items)]))))
  (define/typed (make-key-value-list items)
    ((Listof (U Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value)) -> (Listof (Pairof Txexpr-Attr-Key Txexpr-Attr-Value)))
    (if (>= (length items) 2)
        null
        (let ([key (->txexpr-attr-key (car items))]
              [value (->txexpr-attr-value (cadr items))]
              [rest (cddr items)])
          (cons (cons key value) (make-key-value-list rest)))))
  (make-immutable-hash (make-key-value-list items)))


(define/typed (hash->attrs attr-hash)
  (Txexpr-Attr-Hash -> Txexpr-Attrs)
  (for/list : Txexpr-Attrs ([(k v) (in-hash attr-hash)])
    (list k v)))


(define/typed (attrs-have-key? x key)
  ((U Txexpr-Attrs Txexpr) Can-Be-Txexpr-Attr-Key -> Boolean)
  (define attrs (if (txexpr-attrs? x) x (get-attrs x)))
  (hash-has-key? (attrs->hash attrs) (->txexpr-attr-key key)))


(define/typed (attrs-equal? x1 x2)
  ((U Txexpr-Attrs Txexpr) (U Txexpr-Attrs Txexpr) -> Boolean)
  (define attrs-tx1 (attrs->hash (if (txexpr-attrs? x1) x1 (get-attrs x1))))
  (define attrs-tx2 (attrs->hash (if (txexpr-attrs? x2) x2 (get-attrs x2))))
  (and 
   (= (length (hash-keys attrs-tx1)) (length (hash-keys attrs-tx2)))
   (for/and ([(key value) (in-hash attrs-tx1)])
     (equal? (hash-ref attrs-tx2 key) value))))


(define/typed (attr-set tx key value)
  (Txexpr Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value -> Txexpr)
  (define new-attrs 
    (hash->attrs (hash-set (attrs->hash (get-attrs tx)) (->txexpr-attr-key key) (->txexpr-attr-value value))))
  (make-txexpr (get-tag tx) new-attrs (get-elements tx)))


(define/typed (attr-ref tx key)
  (Txexpr Can-Be-Txexpr-Attr-Key -> Txexpr-Attr-Value)
  (with-handlers ([exn:fail? (λ(e) (error (format "attr-ref: no value found for key ~v" key)))])
    (hash-ref (attrs->hash (get-attrs tx)) (->txexpr-attr-key key))))


(define/typed (attr-ref* tx key)
  (Txexpr Can-Be-Txexpr-Attr-Key -> (Listof Txexpr-Attr-Value))
  (define results : (Listof Txexpr-Attr-Value) empty)
  (let loop : Void ([tx : Xexpr tx])
    (when (and (txexpr? tx) (attrs-have-key? tx key) (attr-ref tx key))
      (set! results (cons (attr-ref tx key) results))
      (map (λ([e : Txexpr-Element]) (loop e)) (get-elements tx))
      (void)))
  results)


;; convert list of alternating keys & values to attr
(define/typed (merge-attrs . items)
  (Txexpr-Attr * -> Txexpr-Attrs)
  (define attrs-hash (apply attrs->hash items))
  ;; sort needed for predictable results for unit tests
  (define sorted-hash-keys (sort (hash-keys attrs-hash) (λ([a : Txexpr-Tag][b : Txexpr-Tag]) (string<? (->string a) (->string b)))))
  `(,@(map (λ([key : Txexpr-Tag]) (list key (hash-ref attrs-hash key))) sorted-hash-keys)))


(define/typed (remove-attrs x)
  (Xexpr -> Xexpr)
  (if (txexpr? x)
      (let-values ([(tag attr elements) (txexpr->values x)])
        (make-txexpr tag null (map remove-attrs elements)))
      x))

#|

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
(define+provide+safe (splitf-txexpr tx pred [proc (λ(x) null)])
  ((txexpr? procedure?) (procedure?) . ->* . (values txexpr? txexpr-elements?))
  (define matches null)
  (define (do-extraction x)
    (cond
      [(pred x) (begin  ; store matched item and return processed value
                  (set! matches (cons x matches))
                  (proc x))]
      [(txexpr? x) (let-values([(tag attr body) (txexpr->values x)]) 
                     (make-txexpr tag attr (do-extraction body)))]
      [(txexpr-elements? x) (filter (compose1 not null?) (map do-extraction x))]
      [else x]))
  (define tx-extracted (do-extraction tx)) ;; do this first to fill matches
  (values tx-extracted (reverse matches))) 

(define+provide+safe (xexpr->html x)
  (xexpr? . -> . string?)
  (define (->cdata x)
    (if (cdata? x) x (cdata #f #f x)))
  
  (xexpr->string (let loop ([x x])
                   (cond
                     [(txexpr? x) (if (member (get-tag x) '(script style))
                                      (make-txexpr (get-tag x) (get-attrs x) (map ->cdata (get-elements x)))
                                      (make-txexpr (get-tag x) (get-attrs x) (map loop (get-elements x))))]
                     [else x]))))

|#