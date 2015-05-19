#lang typed/racket/base
(require (for-syntax racket/base))
(require racket/match racket/string racket/list racket/bool)

(require/typed
 xml
 [valid-char? (Any -> Boolean)]
 [#:struct location ([line : (Option Natural)]
                     [char : (Option Natural)]
                     [offset : Natural])]
 [#:struct source ([start : (U location Symbol #f)]
                   [stop : (U location Symbol #f)])]
 [#:struct (cdata source) ([string : String])]
 [#:struct comment ([text : String])]
 [#:struct (p-i source) ([target-name : Symbol]
                         [instruction : String])])

(define-type Valid-Char Natural) ;; overinclusive but that's as good as it gets
(define-type Txexpr-Tag Symbol)
(define-type Txexpr-Attr-Key Symbol)
(define-type Txexpr-Attr-Value String)
(define-type Txexpr-Attr (Pairof Txexpr-Attr-Key (Pairof Txexpr-Attr-Value Null)))
(define-predicate Txexpr-Attr? Txexpr-Attr)
(define-type Txexpr-Attrs (Listof Txexpr-Attr))
(define-type Txexpr-Element Xexpr)
(define-type Txexpr-Elements (Listof Txexpr-Element))
(define-type Txexpr-Full (List* Txexpr-Tag Txexpr-Attrs (Listof Xexpr)))
(define-type Txexpr-Short (Pairof Txexpr-Tag (Listof Xexpr)))
(define-type Txexpr (U Txexpr-Full Txexpr-Short))
(define-type Xexpr
  (U String
     Txexpr-Full
     Txexpr-Short
     Symbol
     Valid-Char
     cdata
     comment
     p-i))


(define-syntax (define/typed stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(define/typed proc-name type-expr
         (λ(arg ... . rest-arg) body ...))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (: proc-name type-expr)
         (define proc-name body ...))]))

(define/typed (txexpr-tag? x)
  (Any -> Boolean : Txexpr-Tag)
  (symbol? x)) 

(define/typed (txexpr-tags? x)
  (Any -> Boolean : (Listof Txexpr-Tag))
  (and (list? x) (andmap txexpr-tag? x)))

(define/typed (txexpr-attr? x)
  (Any -> Boolean)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))

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


(define/typed (txexpr-attrs? x)
  (Any -> Boolean)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (and (validate-txexpr-attrs x) #t)))


(define/typed (txexpr-elements? x)
  (Any -> Boolean)
  (match x
    [(list elem ...) (andmap txexpr-element? elem)]
    [else #f]))

(define/typed (validate-txexpr-element x #:context [txexpr-context #f])
  ((Any) (#:context Any) . ->* . Txexpr-Element)
  (cond
    [(or (string? x) (txexpr? x) (symbol? x)
         (valid-char? x) (cdata? x)) (cast x Txexpr-Element)]
    [else (error (string-append "validate-txexpr-element: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid element (must be txexpr, string, symbol, XML char, or cdata)" x)))]))


(define/typed (txexpr-element? x)
  (Any -> Boolean)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (and (validate-txexpr-element x) #t)))


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

(define/typed (txexpr? x)
  (Any -> Boolean)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (and (validate-txexpr x) #t)))


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


(define-type Can-Be-Txexpr-Attr-Key (U Symbol String))
(define-type Can-Be-Txexpr-Attr-Value (U Symbol String))

;; helpers. we are getting a string or symbol
(define/typed (->txexpr-attr-key x)
  (Can-Be-Txexpr-Attr-Key -> Txexpr-Attr-Key)
  (if (string? x) (string->symbol x) x))

(define/typed (->txexpr-attr-value x)
  (Can-Be-Txexpr-Attr-Value -> Txexpr-Attr-Value)
  (->string x))

(define/typed (txexpr-attr-key? x) 
  (Any -> Boolean)
  (symbol? x))

(define/typed (can-be-txexpr-attr-key? x)
  (Any -> Boolean)
  (or (symbol? x) (string? x)))

(define/typed (txexpr-attr-value? x) 
  (Any -> Boolean)
  (string? x))

(define/typed (txexpr-attr-values? xs) 
  (Any -> Boolean)
  (and (list? xs) (andmap txexpr-attr-value? xs)))

(define/typed (can-be-txexpr-attr-value? x) 
  (Any -> Boolean)
  (can-be-txexpr-attr-key? x))

(define/typed (->string x)
  ((U Symbol String) -> String)
  (if (symbol? x) (symbol->string x) x))

(define/typed (can-be-txexpr-attrs? x) 
  (Any -> Boolean)
  (ormap (λ([test : (Any -> Boolean)]) (test x)) (list txexpr-attr? txexpr-attrs? can-be-txexpr-attr-key? can-be-txexpr-attr-value?)))

(define/typed (list-of-can-be-txexpr-attrs? xs)
  (Any -> Boolean)
  (and (list? xs) (andmap can-be-txexpr-attrs? xs)))


(define-type Txexpr-Attr-Hash (HashTable Txexpr-Attr-Key Txexpr-Attr-Value))

;; broken: needs flatten
#;(define/typed (attrs->hash . items)
  ((U Txexpr-Attr Txexpr-Attrs) * -> Txexpr-Attr-Hash)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define/typed (make-key-value-list items)
    ((Listof Txexpr-Attr) -> (U Null (Listof (Pairof Txexpr-Attr-Key Txexpr-Attr-Value))))
    (if (null? items)
        null
        (let ([key (->txexpr-attr-key (cast (car items) Can-Be-Txexpr-Attr-Key))]
              [value (->txexpr-attr-value (cast (cadr items) Can-Be-Txexpr-Attr-Value))]
              [rest (cddr items)])
          (cons (cons key value) (make-key-value-list rest)))))
  (make-immutable-hash (make-key-value-list (flatten items))))

;; broken
#;(define/typed (hash->attrs hash)
  (Txexpr-Attr-Hash -> Txexpr-Attrs)
  (hash-map hash list))

;; broken. needs txexpr-attrs? filter to work
#;(define/typed (attrs-have-key? x key)
  ((U Txexpr-Attrs Txexpr) Can-Be-Txexpr-Attr-Key -> Boolean)
  (define attrs (if (txexpr-attrs? x) x (get-attrs x)))
  (hash-has-key? (attrs->hash attrs) (->txexpr-attr-key key)))

;; broken. needs txexpr-attrs? filter to work
#;(define/typed (attrs-equal? x1 x2)
  ((U Txexpr-Attrs Txexpr) (U Txexpr-Attrs Txexpr) -> Boolean)
  (define attrs-tx1 (attrs->hash (if (txexpr-attrs? x1) x1 (get-attrs x1))))
  (define attrs-tx2 (attrs->hash (if (txexpr-attrs? x2) x2 (get-attrs x2))))
  (and 
   (= (length (hash-keys attrs-tx1)) (length (hash-keys attrs-tx2)))
   (for/and ([(key value) (in-hash attrs-tx1)])
     (equal? (hash-ref attrs-tx2 key) value))))

;; broken. needs txexpr-attrs? filter to work
#;(define/typed (attrs-equal? x1 x2)
  ((U Txexpr-Attrs Txexpr) (U Txexpr-Attrs Txexpr) -> Boolean)
  (define attrs-tx1 (attrs->hash (if (txexpr-attrs? x1) x1 (get-attrs x1))))
  (define attrs-tx2 (attrs->hash (if (txexpr-attrs? x2) x2 (get-attrs x2))))
  (and 
   (= (length (hash-keys attrs-tx1)) (length (hash-keys attrs-tx2)))
   (for/and ([(key value) (in-hash attrs-tx1)])
     (equal? (hash-ref attrs-tx2 key) value))))

;; broken. needs hash->attrs
#;(define/typed (attr-set tx key value)
  (Txexpr Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value -> Txexpr)
  (define new-attrs 
    (hash->attrs (hash-set (attrs->hash (get-attrs tx)) (->txexpr-attr-key key) (->txexpr-attr-value value))))
  (make-txexpr (get-tag tx) new-attrs (get-elements tx)))


;; broken: needs attrs->hash
#;(define/typed (attr-ref tx key)
  (Txexpr Can-Be-Txexpr-Attr-Key -> Txexpr-Attr-Value)
  (with-handlers ([exn:fail? (λ(e) (error (format "attr-ref: no value found for key ~v" key)))])
    (hash-ref (attrs->hash (get-attrs tx)) key)))

;; broken: needs attrs-have-key?
#;(define/typed (attr-ref* tx key)
  (Txexpr Can-Be-Txexpr-Attr-Key -> (Listof Txexpr-Attr-Value))
  (filter-not false? 
              (flatten 
               (let loop ([tx tx])
                 (and (txexpr? tx)
                      (cons (and (attrs-have-key? tx key)(attr-ref tx key)) 
                            (map loop (get-elements tx))))))))


;; convert list of alternating keys & values to attr
;; broken: needs attrs->hash
#;(define/typed (merge-attrs . items)
  (Txexpr-Attr * -> Txexpr-Attrs)
  (define attrs-hash (apply attrs->hash items))
  ;; sort needed for predictable results for unit tests
  (define sorted-hash-keys (sort (hash-keys attrs-hash) (λ(a b) (string<? (->string a) (->string b)))))
  `(,@(map (λ(key) (list key (hash-ref attrs-hash key))) sorted-hash-keys)))


;; broken: needs txexpr? filter to work
#;(define/typed (remove-attrs x)
  (Xexpr -> Xexpr)
  (cond
    [(txexpr? x) (let-values ([(tag attr elements) (txexpr->values x)])
                   (make-txexpr tag null (remove-attrs elements)))]
    [(txexpr-elements? x) (map remove-attrs x)]
    [else x]))

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