#lang racket/base
(require sugar/define sugar/coerce racket/string racket/list racket/match xml)
(provide cdata? cdata valid-char? xexpr->string xexpr?) ; from xml

;; Section 2.2 of XML 1.1
;; (XML 1.0 is slightly different and more restrictive)
;; make private version of my-valid-char to get consistent results with Racket 6.0
(define (my-valid-char? i)
  (and (exact-nonnegative-integer? i)
       (or (<= #x1     i #xD7FF)
           (<= #xE000  i #xFFFD)
           (<= #x10000 i #x10FFFF))))


(define (my-xexpr? x)
  (or (txexpr? x) (xexpr? x) (my-valid-char? x)))


(define+provide+safe (txexpr-short? x)
  predicate/c
  (match x
    [(list (? symbol? name) (? my-xexpr?) ...) #t]
    [else #f]))


(define+provide+safe (txexpr? x)
  predicate/c
  (or (txexpr-short? x)
      (match x
        [(list (? symbol?) (list (list (? symbol?) (? string?)) ...) (? my-xexpr?) ...) #t]
        [else #f])))


(define+provide+safe (txexpr-tag? x)
  predicate/c
  (symbol? x))


(define+provide+safe (txexpr-tags? x)
  predicate/c
  (and (list? x) (andmap txexpr-tag? x)))


(define+provide+safe (txexpr-attr? x)
  predicate/c
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))


(define+provide+safe (txexpr-attrs? x)
  predicate/c
  (and (list? x) (andmap txexpr-attr? x)))


(define+provide+safe (txexpr-element? x)
  predicate/c
  (my-xexpr? x))


(define+provide+safe (txexpr-elements? x)
  predicate/c
  (and (list? x) (andmap txexpr-element? x)))


(define+provide+safe (txexpr-attr-key? x)
  predicate/c
  (symbol? x))


(define+provide+safe (can-be-txexpr-attr-key? x)
  predicate/c
  (or (symbol? x) (string? x)))


(define+provide+safe (txexpr-attr-value? x)
  predicate/c
  (string? x))


(define+provide+safe (txexpr-attr-values? x)
  predicate/c
  (and (list? x) (andmap txexpr-attr-value? x)))


(define+provide+safe (can-be-txexpr-attr-value? x)
  predicate/c
  (or (symbol? x) (string? x)))


(define+provide+safe (can-be-txexpr-attrs? x)
  predicate/c
  (ormap (λ(test) (test x)) (list txexpr-attr?
                                  txexpr-attrs?
                                  can-be-txexpr-attr-key?
                                  can-be-txexpr-attr-value?)))


(define+provide+safe (list-of-can-be-txexpr-attrs? xs)
  predicate/c
  (and (list? xs) (andmap can-be-txexpr-attrs? xs)))


(define (validate-txexpr-attrs x #:context [txexpr-context #f])
  (define (make-reason)
    (if (not (list? x)) 
        (format "because ~v is not a list" x)
        (let ([bad-attrs (filter (λ(i) (not (txexpr-attr? i))) x)])
          (format "because ~a ~a" (string-join (map (λ(ba) (format "~v" ba)) bad-attrs) " and ")
                  (if (> (length bad-attrs) 1)
                      "are not valid attributes"
                      "is not in the form '(symbol \"string\")")))))
  (cond
    [(and (list? x) (> (length x) 0) (andmap txexpr-attr? x)) x]
    [else (error (string-append "validate-txexpr-attrs: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid list of attributes ~a" x
                                        (make-reason))))]))


(define (validate-txexpr-element x #:context [txexpr-context #f])
  (cond
    [(or (string? x) (txexpr? x) (symbol? x)
         (valid-char? x) (cdata? x)) x]
    [else (error (string-append "validate-txexpr-element: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid element (must be txexpr, string, symbol, XML char, or cdata)" x)))]))


;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define+provide+safe (validate-txexpr x)
  (any/c . -> . txexpr?)
  (define-syntax-rule (validate-txexpr-attrs-with-context e)
    (validate-txexpr-attrs e #:context x))
  (define-syntax-rule (validate-txexpr-element-with-context e)
    (validate-txexpr-element e #:context x))
  (cond
    [(txexpr-short? x) x]
    [(txexpr? x) (and
                  (validate-txexpr-attrs-with-context (get-attrs x))
                  (andmap (λ(e) (validate-txexpr-element-with-context e)) (get-elements x)) x)]
    [(and (list? x) (symbol? (car x)))
     (and
      (andmap (λ(e) (validate-txexpr-element-with-context e)) (get-elements x))
      (validate-txexpr-attrs-with-context (get-attrs x)))]
    [(list? x) (error 'validate-txexpr (format "~v is a list but it doesn't start with a symbol" x))]
    [else (error 'validate-txexpr (format "~v: not an X-expression" x))]))


(define+provide+safe (txexpr tag [attrs null] [elements null])
  ((symbol?) (txexpr-attrs? txexpr-elements?) . ->* . txexpr?)
  (define result (cons tag (append (if (empty? attrs) empty (list attrs)) elements)))
  (if (txexpr? result)
      result
      (error 'txexpr
             (cond
               [(not (txexpr-tag? tag))
                (format "This is not a txexpr-tag: ~v" tag)]
               [(not (txexpr-attrs? attrs))
                (format "This is not a list of txexpr-attrs: ~v" attrs)]
               [(not (txexpr-elements? elements))
                (format "This is not a list of txexpr-elements: ~v" elements)]
               [else ""]))))

(define make-txexpr txexpr)
(provide+safe make-txexpr)

(define+provide+safe (txexpr->values x)
  (txexpr? . -> . (values symbol? txexpr-attrs? txexpr-elements?))
  (if (txexpr-short? x)
      (values (car x) '() (cdr x))
      (values (car x) (cadr x) (cddr x))))


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
  (txexpr? . -> . txexpr-elements?)
  (define-values (tag attrs elements) (txexpr->values x))
  elements)


;; helpers. we are getting a string or symbol
(define+provide+safe (->txexpr-attr-key x)
  (can-be-txexpr-attr-key? . -> . txexpr-attr-key?)
  (if (string? x) (string->symbol x) x))


(define+provide+safe (->txexpr-attr-value x)
  (can-be-txexpr-attr-value? . -> . txexpr-attr-value?)
  (->string x))


(define+provide+safe (attrs->hash . items-in)
  (() #:rest (listof can-be-txexpr-attrs?) . ->* . hash-eq?)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define items (reverse
                 (for/fold ([items null]) ([i (in-list items-in)])
                   (cond
                     [(txexpr-attr? i) (append (reverse i) items)]
                     [(txexpr-attrs? i) (append (append* (map (λ(a) (reverse a)) i)) items)]
                     [else (cons i items)]))))
  (define (make-key-value-list items)
    (if (< (length items) 2)
        null
        (let ([key (->txexpr-attr-key (car items))]
              [value (->txexpr-attr-value (cadr items))]
              [rest (cddr items)])
          (cons (cons key value) (make-key-value-list rest)))))
  (make-immutable-hasheq (make-key-value-list items)))


(define+provide+safe (hash->attrs attr-hash)
  (hash? . -> . txexpr-attrs?)
  (map (λ(k) (list k (hash-ref attr-hash k))) (hash-keys attr-hash)))


(define+provide+safe (attrs-have-key? x key)
  ((or/c txexpr-attrs? txexpr?) can-be-txexpr-attr-key? . -> . boolean?)
  (define attrs (if (txexpr-attrs? x) x (get-attrs x)))
  (hash-has-key? (attrs->hash attrs) (->txexpr-attr-key key)))


(define+provide+safe (attrs-equal? x1 x2)
  ((or/c txexpr-attrs? txexpr?) (or/c txexpr-attrs? txexpr?) . -> . boolean?)
  (define attrs-tx1 (attrs->hash (if (txexpr-attrs? x1) x1 (get-attrs x1))))
  (define attrs-tx2 (attrs->hash (if (txexpr-attrs? x2) x2 (get-attrs x2))))
  (and 
   (= (length (hash-keys attrs-tx1)) (length (hash-keys attrs-tx2)))
   (for/and ([(key value) (in-hash attrs-tx1)])
            (equal? (hash-ref attrs-tx2 key) value))))


(define+provide+safe (attr-set tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (define new-attrs 
    (hash->attrs (hash-set (attrs->hash (get-attrs tx))
                           (->txexpr-attr-key key)
                           (->txexpr-attr-value value))))
  (make-txexpr (get-tag tx) new-attrs (get-elements tx)))


(define+provide+safe (attr-join tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (define starting-values (string-split (if (attrs-have-key? tx key)
                                            (attr-ref tx key)
                                            "")))
  (attr-set tx key (string-join `(,@starting-values ,value) " ")))      


(define+provide+safe (attr-set* tx . kvs)
  ((txexpr?) #:rest (listof (or/c can-be-txexpr-attr-key? can-be-txexpr-attr-value?)) . ->* . txexpr?)
  (define attrs-to-set (apply hash kvs))
  (foldl (λ(kv-pair acc-tx) (attr-set acc-tx (car kv-pair) (cdr kv-pair))) tx (hash->list attrs-to-set)))


(define+provide+safe (attr-ref tx key)
  (txexpr? can-be-txexpr-attr-key? . -> . txexpr-attr-value?)
  (with-handlers ([exn:fail? (λ(e) (error (format "attr-ref: no value found for key ~v" key)))])
    (hash-ref (attrs->hash (get-attrs tx)) (->txexpr-attr-key key))))


(define+provide+safe (attr-ref* tx key)
  (txexpr? can-be-txexpr-attr-key? . -> . txexpr-attr-values?)
  (define results empty)
  (let loop ([tx tx])
    (when (and (txexpr? tx) (attrs-have-key? tx key) (attr-ref tx key))
      (set! results (cons (attr-ref tx key) results))
      (map (λ(e) (loop e)) (get-elements tx))
      (void)))
  (reverse results))


;; convert list of alternating keys & values to attr
(define+provide+safe (merge-attrs . items)
  (() #:rest list-of-can-be-txexpr-attrs? . ->* . txexpr-attrs?)
  (define attrs-hash (apply attrs->hash items))
  ;; sort needed for predictable results for unit tests
  (define sorted-hash-keys (sort (hash-keys attrs-hash)
                                 (λ(a b) (string<? (->string a) (->string b)))))
  `(,@(map (λ(key) (list key (hash-ref attrs-hash key))) sorted-hash-keys)))


(define+provide+safe (remove-attrs x)
  (txexpr? . -> . txexpr?)
  (if (txexpr? x)
      (let-values ([(tag attr elements) (txexpr->values x)])
        (make-txexpr tag null (map remove-attrs elements)))
      x))


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
(define deleted-signal (gensym))
(define+provide+safe (splitf-txexpr tx pred [proc (λ(x) deleted-signal)])
  ((txexpr? procedure?) (procedure?) . ->* . (values txexpr? txexpr-elements?))
  (define matches null)
  (define (do-extraction x)
    (cond
      [(pred x) (begin  ; store matched item and return processed value
                  (set! matches (cons x matches))
                  (proc x))]
      [(txexpr? x) (let-values([(tag attr elements) (txexpr->values x)]) 
                     (make-txexpr tag attr (filter (λ(e) (not (equal? e deleted-signal)))
                                                   (map do-extraction elements))))]
      [else x]))
  (define tx-extracted (do-extraction tx)) ;; do this first to fill matches
  (values (if (txexpr? tx-extracted)
              tx-extracted
              (error 'splitf-txexpr "Bad input")) (reverse matches)))


(define+provide+safe (xexpr->html x)
  (xexpr? . -> . string?)
  (define (->cdata x)
    (cond
      [(cdata? x) x]
      ; don't use "![CDATA[...]]" wrapper in HTML, it's not consistent with the spec
      [(string? x) (cdata #f #f x)] 
      [else x]))
  (xexpr->string (let loop ([x x])
                   (cond
                     [(txexpr? x) (if (member (get-tag x) '(script style))
                                      (make-txexpr (get-tag x) (get-attrs x)
                                                   (map ->cdata (get-elements x)))
                                      (make-txexpr (get-tag x) (get-attrs x)
                                                   (map loop (get-elements x))))]
                     [else x]))))

(require rackunit)
(provide+safe check-txexprs-equal?)
(define-simple-check (check-txexprs-equal? tx1 tx2)
  ;; txexprs are deemed equal if they differ only in the ordering of attributes.
  ;; therefore, to check them, 1) sort their attributes, 2) straight list comparison.
  ;; use letrec because `define-simple-check` wants an expression in <=6.2
  
  ;; `stringify-attr` is needed because comparing attr keys won't work if there are two attrs with same key.
  ;; so the whole attr is converted into a single string for sorting, which lets the attr value act as a tiebreaker.
  ;; it doesn't matter that this sort may not be correct (in the sense of a desirable ordering)
  ;; it just needs to be stable (e.g., a certain set of attrs will always sort the same way)
  (letrec ([stringify-attr (λ(attr) (string-append (symbol->string (car attr)) (cadr attr)))]
           [sort-attrs (λ(x)
                         (if (txexpr? x)
                             (let-values ([(tag attr elements) (txexpr->values x)])
                               (make-txexpr tag (sort attr #:key stringify-attr #:cache-keys? #t string<?) (map sort-attrs elements)))
                             x))])
    (equal? (sort-attrs tx1) (sort-attrs tx2))))

(check-txexprs-equal? '(p ((b "foo")(a "bar")) (span ((d "foo")(c "bar"))))
                      '(p ((a "bar")(b "foo")) (span ((c "bar")(d "foo")))))
;; two attrs with same key
(check-txexprs-equal? '(p ((a "foo")(a "bar")))
                      '(p ((a "bar")(a "foo"))))