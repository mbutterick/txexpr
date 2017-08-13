#lang racket/base
(require sugar/define sugar/coerce racket/string racket/list xml)
(provide cdata? cdata valid-char? xexpr->string xexpr?) ; from xml
(provide empty) ; from racket/list


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


(define+provide+safe (txexpr? x [short-only #f])
  predicate/c
  (and (pair? x)
       (txexpr-tag? (car x))
       (let ([result (or (and (empty? (cdr x)) 'short)
                         ;; separate the my-xexpr? tail match from the rest.
                         ;; as a recursive operation, it's potentially time-consuming.
                         (and (andmap my-xexpr? (cddr x))
                              (cond 
                                [(txexpr-attrs? (cadr x)) #t]
                                [(my-xexpr? (cadr x)) 'short]
                                [else #f])))])
         (and result (if short-only
                         (eq? result 'short)
                         #t)))))


(define+provide+safe (txexpr-short? x)
  predicate/c
  (txexpr? x 'short-only))


(define+provide+safe (txexpr-tag? x)
  predicate/c
  (symbol? x))


(define+provide+safe (txexpr-attr? x)
  predicate/c
  (and (list? x)
       (= 2 (length x))
       (txexpr-attr-key? (first x))
       (txexpr-attr-value? (second x))))


(define+provide+safe (txexpr-element? x)
  predicate/c
  (my-xexpr? x))


(define+provide+safe (txexpr-attr-key? x)
  predicate/c
  (symbol? x))


(define+provide+safe (can-be-txexpr-attr-key? x)
  predicate/c
  (symbolish? x))


(define+provide+safe (txexpr-attr-value? x)
  predicate/c
  (string? x))


(define+provide+safe (can-be-txexpr-attr-value? x)
  predicate/c
  (stringish? x))


(define-syntax-rule (define-plural plural-id pred)
  (define+provide+safe (plural-id x)
    predicate/c
    (and (list? x) (andmap pred x))))


(define-plural txexpr-tags? txexpr-tag?)
(define-plural txexpr-attrs? txexpr-attr?)
(define-plural txexpr-elements? txexpr-element?)
(define-plural txexpr-attr-values? txexpr-attr-value?)
(define-plural list-of-can-be-txexpr-attrs? can-be-txexpr-attrs?)


(define+provide+safe (can-be-txexpr-attrs? x)
  predicate/c
  (for/or ([test (in-list (list txexpr-attr?
                                txexpr-attrs?
                                can-be-txexpr-attr-key?
                                can-be-txexpr-attr-value?))])
          (test x)))


(define (validate-txexpr-attrs x #:context [txexpr-context #f])
  (define (make-reason)
    (if (not (list? x)) 
        (format "because ~v is not a list" x)
        (let ([bad-attrs (filter (λ (i) (not (txexpr-attr? i))) x)])
          (format "because ~a ~a" (string-join (map (λ (ba) (format "~v" ba)) bad-attrs) " and ")
                  (if (> (length bad-attrs) 1)
                      "are not valid attributes"
                      "is not in the form '(symbol \"string\")")))))
  (cond
    [(and (list? x) (positive? (length x)) (andmap txexpr-attr? x)) x]
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
  (cond
    [(txexpr-short? x) x]
    [(txexpr? x) (and
                  (validate-txexpr-attrs (get-attrs x) #:context x)
                  (andmap (λ (e) (validate-txexpr-element e #:context x)) (get-elements x)) x)]
    [(and (list? x) (symbol? (car x)))
     (and
      (andmap (λ (e) (validate-txexpr-element e #:context x)) (get-elements x))
      (validate-txexpr-attrs (get-attrs x) #:context x))]
    [(list? x) (error 'validate-txexpr (format "~v is a list but it doesn't start with a symbol" x))]
    [else (error 'validate-txexpr (format "~v: not an X-expression" x))]))


(define (txexpr-unsafe tag attrs elements)
  (cons tag (if (empty? attrs)
                elements
                (cons attrs elements))))


(define (txexpr-base func-name tag attrs elements)
  (unless (txexpr-tag? tag)
    (raise-argument-error func-name "txexpr-tag?" tag))
  (unless (txexpr-attrs? attrs)
    (raise-argument-error func-name "txexpr-attrs?" attrs))
  (unless (txexpr-elements? elements)
    (raise-argument-error func-name "txexpr-elements?" elements))
  (txexpr-unsafe tag attrs elements))


(define+provide+safe (txexpr tag [attrs null] [elements null])
  ((txexpr-tag?) (txexpr-attrs? txexpr-elements?) . ->* . txexpr?)
  (txexpr-base 'txexpr tag attrs elements))


(define+provide+safe (txexpr* tag [attrs null] . elements)
  ((txexpr-tag?) (txexpr-attrs?) #:rest txexpr-elements? . ->* . txexpr?)
  (txexpr-base 'txexpr* tag attrs elements))


(define make-txexpr txexpr) ; for backward compatability
(provide+safe make-txexpr)


(define+provide+safe (txexpr->values x)
  (txexpr? . -> . (values txexpr-tag? txexpr-attrs? txexpr-elements?))
  (if (txexpr-short? x)
      (values (car x) '() (cdr x))
      (values (car x) (cadr x) (cddr x))))


(define+provide+safe (txexpr->list x)
  (txexpr? . -> . list?)
  (call-with-values (λ () (txexpr->values x)) list))


;; convenience functions to retrieve only one part of txexpr
(define+provide+safe (get-tag x)
  (txexpr? . -> . txexpr-tag?)
  (car x))


(define+provide+safe (get-attrs x)
  (txexpr? . -> . txexpr-attrs?)
  (define-values (tag attrs elements) (txexpr->values x))
  attrs)


(define+provide+safe (get-elements x)
  (txexpr? . -> . txexpr-elements?)
  (define-values (tag attrs elements) (txexpr->values x))
  elements)


;; helpers. we are getting a string or symbol
(define+provide+safe (->txexpr-attr-key x)
  (can-be-txexpr-attr-key? . -> . txexpr-attr-key?)
  (unless (can-be-txexpr-attr-key? x)
    (raise-argument-error '->txexpr-attr-key "can-be-txexpr-attr-key?" x))
  (->symbol x))


(define+provide+safe (->txexpr-attr-value x)
  (can-be-txexpr-attr-value? . -> . txexpr-attr-value?)
  (unless (can-be-txexpr-attr-value? x)
      (raise-argument-error '->txexpr-attr-value "can-be-txexpr-attr-value?" x))
  (->string x))


(define identity (λ (x) x))
(define+provide+safe (attrs->hash #:hash-style? [hash-style-priority #f] . items-in)
  (() (#:hash-style? boolean?) #:rest (listof can-be-txexpr-attrs?) . ->* . hash-eq?)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define items (flatten items-in))
  (unless (even? (length items))
    (raise-argument-error 'attrs->hash "argument list of even length" (length items-in)))
  ;; hasheq loop will overwrite earlier values with later.
  ;; but earlier attributes need priority (see https://www.w3.org/TR/xml/#attdecls)
  ;; thus reverse the pairs.
  ;; priority-inverted will defeat this assumption, and allow later attributes to overwrite earlier.
  (for/hasheq ([sublist (in-list ((if hash-style-priority
                                      identity
                                      reverse) (for/list (#:when (pair? items)
                                                          [(k ki) (in-indexed items)]
                                                          [v (in-list (cdr items))]
                                                          #:when (even? ki))
                                                         (list k v))))])
              (let ([key (first sublist)]
                    [value (second sublist)])
                (values (->txexpr-attr-key key) (->txexpr-attr-value value)))))


(define+provide+safe (hash->attrs attr-hash)
  (hash? . -> . txexpr-attrs?)
  (map flatten (hash->list attr-hash)))


(define+provide+safe (attrs-have-key? x key)
  ((or/c txexpr-attrs? txexpr?) can-be-txexpr-attr-key? . -> . boolean?)
  (define attrs (if (txexpr-attrs? x) x (get-attrs x)))
  (and (assq (->txexpr-attr-key key) attrs) #t))


(define+provide+safe (attr-set tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (attr-set* tx key value))


(define+provide+safe (attr-set* tx . kvs)
  ((txexpr?) #:rest (listof (or/c can-be-txexpr-attr-key? can-be-txexpr-attr-value?)) . ->* . txexpr?)
  ;; unlike others, this uses hash operations to guarantee that your attr-set
  ;; is the only one remaining.
  (unless (even? (length kvs))
    (raise-argument-error 'attr-set* "even number of arguments" kvs))
  (define new-attrs 
    (hash->attrs
     (apply hash-set* (attrs->hash (get-attrs tx))
            (append-map (λ (sublist)
                          (list (->txexpr-attr-key (first sublist))
                                (->txexpr-attr-value (second sublist))))
                        (let ([items kvs])
                          (for/list (#:when (pair? items)
                                     [(k ki) (in-indexed items)]
                                     [v (in-list (cdr items))]
                                     #:when (even? ki))
                                    (list k v)))))))
  (txexpr-base 'attr-set* (get-tag tx) new-attrs (get-elements tx)))


(define+provide+safe (attr-join tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (define starting-values (string-split (if (attrs-have-key? tx key)
                                            (attr-ref tx key)
                                            "")))
  (attr-set tx key (string-join (append starting-values (list value)) " ")))      


(define+provide+safe (attr-ref tx key [failure-result (λ _ (raise (make-exn:fail:contract (format "attr-ref: no value found for key ~v" key) (current-continuation-marks))))])
  ((txexpr? can-be-txexpr-attr-key?) (any/c) . ->* . any)
  (define result (assq (->txexpr-attr-key key) (get-attrs tx)))
  (if result
      (second result)
      (if (procedure? failure-result)
          (failure-result)
          failure-result)))


(define+provide+safe (remove-attrs x)
  (txexpr? . -> . txexpr?)
  (let loop ([x x])
    (if (txexpr? x)
        (let-values ([(tag attrs elements) (txexpr->values x)])
          (cons tag (map loop elements)))
        x)))


(define+provide+safe (map-elements proc x)
  (procedure? txexpr? . -> . txexpr?)
  (proc (if (txexpr? x) 
            (let-values ([(tag attrs elements) (txexpr->values x)])
              (txexpr-unsafe tag attrs (map (λ (e)(map-elements proc e)) elements)))
            x)))


;; function to split tag out of txexpr
(define deleted-signal (gensym))
(define+provide+safe (splitf-txexpr tx pred [proc (λ (x) deleted-signal)])
  ((txexpr? procedure?) (procedure?) . ->* . (values txexpr? txexpr-elements?))
  (unless (txexpr? tx)
    (raise-argument-error 'splitf-txexpr "txexpr?" tx))
  (define matches null)
  (define (extract! x)
    (cond
      [(pred x) ;; store matched item and return processed value
       (set! matches (cons x matches))
       (proc x)]
      [(txexpr? x) (let-values([(tag attrs elements) (txexpr->values x)]) 
                     (txexpr tag attrs (filter (λ (e) (not (eq? e deleted-signal)))
                                               (map extract! elements))))]
      [else x]))
  (define tx-extracted (extract! tx)) ;; do this first to fill matches
  (values tx-extracted (reverse matches)))


(define+provide+safe (findf*-txexpr tx pred)
  (txexpr? procedure? . -> . (or/c #f txexpr-elements?))
  (define-values (_ matches) (splitf-txexpr tx pred))
  (and (pair? matches) matches))


(define+provide+safe (findf-txexpr tx pred)
  (txexpr? procedure? . -> . (or/c #f txexpr-element?))
  (define matches (findf*-txexpr tx pred))
  (and matches (car matches)))


;; don't use "![CDATA[...]]" wrapper in HTML, it's not consistent with the spec
(define (->cdata x)
  (if (string? x)
      (cdata #f #f x)
      x))


(define+provide+safe (xexpr->html x)
  (xexpr? . -> . string?)
  (xexpr->string
   (let loop ([x x])
     (if (txexpr? x)
         (let*-values ([(tag attrs elements) (txexpr->values x)]
                       [(proc) (if (memq tag '(script style))
                                   ->cdata
                                   loop)])
           ;; a little faster than `txexpr` since we know the pieces are valid
           (txexpr-unsafe tag attrs (map proc elements)))
         x))))
