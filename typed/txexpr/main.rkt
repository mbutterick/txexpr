#lang typed/racket/base
(require (for-syntax typed/racket/base) typed/sugar/define)
(require racket/match racket/string racket/list racket/bool "core-predicates.rkt")
(provide (all-defined-out) (all-from-out "core-predicates.rkt"))
(require typed/sugar/debug)

(define/typed (validate-txexpr-attrs x #:context [txexpr-context #f])
  (Txexpr-Attrs [#:context Any]  -> Txexpr-Attrs)
  (define/typed (make-reason)
    (-> String)
    (if (not (list? x)) 
        (format "because ~v is not a list" x)
        (let ([bad-attrs (filter (λ(i) (not (txexpr-attr? i))) x)])
          (format "because ~a ~a" (string-join (map (λ(ba) (format "~v" ba)) bad-attrs) " and ") (if (> (length bad-attrs) 1)
                                                                                                     "are not valid attributes"
                                                                                                     "is not in the form '(symbol \"string\")")))))
  (cond
    [(and (list? x) (> (length x) 0) (andmap txexpr-attr? x)) x]
    [else (error (string-append "validate-txexpr-attrs: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid list of attributes ~a" x (make-reason))))]))


(define/typed (validate-txexpr-element x #:context [txexpr-context #f])
  (Txexpr-Element [#:context Any] -> Txexpr-Element)
  (cond
    [(or (string? x) (txexpr? x) (symbol? x)
         (valid-char? x) (cdata? x)) x]
    [else (error (string-append "validate-txexpr-element: "
                                (if txexpr-context (format "in ~v, " txexpr-context) "")
                                (format "~v is not a valid element (must be txexpr, string, symbol, XML char, or cdata)" x)))]))


;; is it a named x-expression?
;; todo: rewrite this recurively so errors can be pinpointed (for debugging)
(define/typed (validate-txexpr x)
  (Any -> (Option Txexpr))
  (define-syntax-rule (validate-txexpr-attrs-with-context e) (validate-txexpr-attrs e #:context x))
  (define-syntax-rule (validate-txexpr-element-with-context e) (validate-txexpr-element e #:context x))
  (cond
    [(txexpr-short? x) x]
    [(txexpr? x) (and
                  (validate-txexpr-attrs-with-context (get-attrs x))
                  (andmap (λ:([e : Txexpr-Element]) (validate-txexpr-element-with-context e)) (get-elements x)) x)]
    [else (error 'validate-txexpr (format "~v is not a list starting with a symbol" x))]))


(define/typed make-txexpr
  (case-> (Symbol -> Txexpr)
          (Symbol Txexpr-Attrs -> Txexpr)
          (Symbol Txexpr-Attrs (Listof Txexpr-Element) -> Txexpr))
  (case-lambda
    [(tag) (make-txexpr tag null null)]
    [(tag attrs) (make-txexpr tag attrs null)]
    [(tag attrs elements)
     (define result (cons tag (append (if (empty? attrs) empty (list attrs)) elements)))
     (if (txexpr? result)
         result
         (error 'make-txexpr "This can't happen"))]))


(define/typed (txexpr->values x)
  (Txexpr -> (values Txexpr-Tag Txexpr-Attrs Txexpr-Elements))
  (if (txexpr-short? x)
      (values (car x) '() (cdr x))
      (values (car x) (cadr x) (cddr x))))


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
                 (for/fold: ([items : (Listof (U Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value)) null])
                   ([i (in-list items-in)])
                   (cond
                     [(txexpr-attr? i) (append (reverse i) items)]
                     [(txexpr-attrs? i) (append (append* (map (λ:([a : Txexpr-Attr]) (reverse a)) i)) items)]
                     [else (cons i items)]))))
  (define/typed (make-key-value-list items)
    ((Listof (U Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value)) -> (Listof (Pairof Txexpr-Attr-Key Txexpr-Attr-Value)))
    (if (< (length items) 2)
        null
        (let ([key (->txexpr-attr-key (car items))]
              [value (->txexpr-attr-value (cadr items))]
              [rest (cddr items)])
          (cons (cons key value) (make-key-value-list rest)))))
  (make-immutable-hash (make-key-value-list items)))


(define/typed (hash->attrs attr-hash)
  (Txexpr-Attr-Hash -> Txexpr-Attrs)
  (map (λ:([k : Txexpr-Attr-Key]) (list k (hash-ref attr-hash k))) (hash-keys attr-hash)))


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
  (define: results : (Listof Txexpr-Attr-Value) empty)
  (let: loop : Void ([tx : Xexpr tx])
    (when (and (txexpr? tx) (attrs-have-key? tx key) (attr-ref tx key))
      (set! results (cons (attr-ref tx key) results))
      (map (λ:([e : Txexpr-Element]) (loop e)) (get-elements tx))
      (void)))
  (reverse results))


;; convert list of alternating keys & values to attr
(define/typed (merge-attrs . items)
  (Can-Be-Txexpr-Attr * -> Txexpr-Attrs)
  (define attrs-hash (apply attrs->hash items))
  ;; sort needed for predictable results for unit tests
  (define sorted-hash-keys (sort (hash-keys attrs-hash) (λ:([a : Txexpr-Tag][b : Txexpr-Tag]) (string<? (->string a) (->string b)))))
  `(,@(map (λ:([key : Txexpr-Tag]) (list key (hash-ref attrs-hash key))) sorted-hash-keys)))


(define/typed (remove-attrs x)
  (Xexpr -> Xexpr)
  (if (txexpr? x)
      (let-values ([(tag attr elements) (txexpr->values x)])
        (make-txexpr tag null (map remove-attrs elements)))
      x))



(define/typed (map-elements/exclude proc x exclude-test)
  ((Xexpr -> Xexpr) Xexpr (Xexpr -> Boolean) -> Xexpr)
  (cond
    [(txexpr? x) 
     (if (exclude-test x)
         x
         (let-values ([(tag attr elements) (txexpr->values x)])
           (make-txexpr tag attr 
                        (map (λ:([x : Xexpr])(map-elements/exclude proc x exclude-test)) elements))))]
    ;; externally the function only accepts txexpr,
    ;; but internally we don't care
    [else (proc x)]))


(define/typed (map-elements proc x)
  ((Xexpr -> Xexpr) Xexpr -> Xexpr)
  (map-elements/exclude proc x (λ(x) #f)))


;; function to split tag out of txexpr
(define deleted-signal (gensym))
(define/typed splitf-txexpr
  (case-> (Txexpr (Xexpr -> Boolean) -> (values Txexpr Txexpr-Elements))
          (Txexpr (Xexpr -> Boolean) (Xexpr -> Xexpr) -> (values Txexpr Txexpr-Elements)))
  (case-lambda
    [(tx pred) (splitf-txexpr tx pred (λ:([x : Xexpr]) deleted-signal))]
    [(tx pred proc)
     (define: matches : Txexpr-Elements null)
     (define/typed (do-extraction x)
       (Xexpr -> Xexpr)
       (cond
         [(pred x) (begin  ; store matched item and return processed value
                     (set! matches (cons x matches))
                     (proc x))]
         [(txexpr? x) (let-values([(tag attr elements) (txexpr->values x)]) 
                        (make-txexpr tag attr (filter (λ:([e : Xexpr]) (not (equal? e deleted-signal))) (map do-extraction elements))))]
         [else x]))
     (define: tx-extracted : Xexpr (do-extraction tx)) ;; do this first to fill matches
     (values (if (txexpr? tx-extracted)
                 tx-extracted
                 (error 'splitf-txexpr "Can't get here")) (reverse matches))]))


(define/typed (xexpr->html x)
  (Xexpr -> String)
  (define/typed (->cdata x)
    (Xexpr -> Xexpr)
    (cond
      [(cdata? x) x]
      [(string? x) (cdata #f #f  (format "<![CDATA[~a]]>" x))]
      [else x]))
  (xexpr->string (let: loop : Xexpr ([x : Xexpr x])
                   (cond
                     [(txexpr? x) (if (member (get-tag x) '(script style))
                                      (make-txexpr (get-tag x) (get-attrs x) (map ->cdata (get-elements x)))
                                      (make-txexpr (get-tag x) (get-attrs x) (map loop (get-elements x))))]
                     [else x]))))

