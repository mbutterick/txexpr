#lang racket/base
(require sugar/define sugar/coerce sugar/list racket/string racket/list racket/match xml rackunit)
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


(define+provide+safe (txexpr? x #:short-only? [short-only #f])
  predicate/c
  (define short-sym 'short)
  (and (pair? x)
       (txexpr-tag? (car x))
       (let ([result (or (and (empty? (cdr x)) short-sym)
                         ;; separate the my-xexpr? tail match from the rest.
                         ;; as a recursive operation, it's potentially time-consuming.
                         (and (andmap my-xexpr? (cddr x))
                              (match (cadr x)
                                [(list (? txexpr-attr?) ...) #t]
                                [(? my-xexpr?) short-sym]
                                [else #f])))])
         (and result (if short-only
                         (eq? result short-sym)
                         #t)))))


(define+provide+safe (txexpr-short? x)
  predicate/c
  (txexpr? x #:short-only? #t))


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


(define+provide+safe (txexpr-element? x)
  predicate/c
  (my-xexpr? x))


(define+provide+safe (txexpr-attr-key? x)
  predicate/c
  (symbol? x))


(define+provide+safe (can-be-txexpr-attr-key? x)
  predicate/c
  (or (symbol? x) (string? x)))


(define+provide+safe (txexpr-attr-value? x)
  predicate/c
  (string? x))


(define+provide+safe (can-be-txexpr-attr-value? x)
  predicate/c
  (or (symbol? x) (string? x)))


(define-syntax-rule (define-plural plural-id pred)
  (define+provide+safe (plural-id x)
    predicate/c
    (and (list? x) (andmap pred x))))

(define-plural txexpr-attrs? txexpr-attr?)
(define-plural txexpr-elements? txexpr-element?)
(define-plural txexpr-attr-values? txexpr-attr-value?)
(define-plural list-of-can-be-txexpr-attrs? can-be-txexpr-attrs?)


(define+provide+safe (can-be-txexpr-attrs? x)
  predicate/c
  (ormap (λ(test) (test x)) (list txexpr-attr?
                                  txexpr-attrs?
                                  can-be-txexpr-attr-key?
                                  can-be-txexpr-attr-value?)))


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


(define make-txexpr txexpr) ; for backward compatability
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
  (->symbol x))


(define+provide+safe (->txexpr-attr-value x)
  (can-be-txexpr-attr-value? . -> . txexpr-attr-value?)
  (->string x))

(define identity (λ (x) x))
(define+provide+safe (attrs->hash #:hash-style? [hash-style-priority #f] . items-in)
  (() (#:hash-style? boolean?) #:rest (listof can-be-txexpr-attrs?) . ->* . hash-eq?)
  ;; can be liberal with input because they're all just nested key/value pairs
  ;; but still need this function to make sure that 'foo and "foo" are treated as the same hash key
  (define items (flatten items-in))
  (unless (even? (length items))
    (raise-argument-error 'attrs->hash "even number of arguments" items-in))
  ;; hasheq loop will overwrite earlier values with later.
  ;; but earlier attributes need priority (see https://www.w3.org/TR/xml/#attdecls)
  ;; thus reverse the pairs.
  ;; priority-inverted will defeat this assumption, and allow later attributes to overwrite earlier.
  (for/hasheq ([sublist (in-list ((if hash-style-priority
                                      identity
                                      reverse) (slice-at items 2)))])
              (let ([key (->txexpr-attr-key (first sublist))]
                    [value (->txexpr-attr-value (second sublist))])
                (values key value))))


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
            (append-map (λ(sublist)
                          (list (->txexpr-attr-key (first sublist))
                                (->txexpr-attr-value (second sublist)))) (slice-at kvs 2)))))
  (txexpr (get-tag tx) new-attrs (get-elements tx)))



(define+provide+safe (attr-join tx key value)
  (txexpr? can-be-txexpr-attr-key? can-be-txexpr-attr-value? . -> . txexpr?)
  (define starting-values (string-split (if (attrs-have-key? tx key)
                                            (attr-ref tx key)
                                            "")))
  (attr-set tx key (string-join `(,@starting-values ,value) " ")))      



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
        (let-values ([(tag attr elements) (txexpr->values x)])
          (txexpr tag null (map loop elements)))
        x)))



(define+provide+safe (map-elements proc x)
  (procedure? txexpr? . -> . txexpr?)
  (proc (if (txexpr? x) 
            (let-values ([(tag attr elements) (txexpr->values x)])
              (txexpr tag attr (map (λ(e)(map-elements proc e)) elements)))
            x)))


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
                     (txexpr tag attr (filter (λ(e) (not (equal? e deleted-signal)))
                                              (map do-extraction elements))))]
      [else x]))
  (define tx-extracted (do-extraction tx)) ;; do this first to fill matches
  (unless (txexpr? tx-extracted)
    (error 'splitf-txexpr "Bad input"))
  (values tx-extracted (reverse matches)))


(define+provide+safe (findf*-txexpr tx pred)
  (txexpr? procedure? . -> . (or/c #f txexpr-elements?))
  (define-values (_ matches) (splitf-txexpr tx pred))
  (and (pair? matches) matches))


(define+provide+safe (findf-txexpr tx pred)
  (txexpr? procedure? . -> . (or/c #f txexpr-element?))
  (define matches (findf*-txexpr tx pred))
  (and matches (car matches)))


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
                                      (txexpr (get-tag x) (get-attrs x)
                                              (map ->cdata (get-elements x)))
                                      (txexpr (get-tag x) (get-attrs x)
                                              (map loop (get-elements x))))]
                     [else x]))))

