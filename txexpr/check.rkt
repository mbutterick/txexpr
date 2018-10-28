#lang racket/base
(require sugar/define racket/match "base.rkt" rackunit)

(define (stringify-attr attr)
  (match attr
    [(list key val) (string-append (symbol->string key) val)]))

(define (sort-attrs x)
  (match x
    [(? txexpr?)
     (let-values ([(tag attr elements) (txexpr->values x)])
       (txexpr tag (sort attr #:key stringify-attr #:cache-keys? #t string<?) (map sort-attrs elements)))]
    [_ x]))

(define (txexprs-equal? tx1 tx2)
  ;; txexprs are deemed equal if they differ only in the ordering of attributes.
  ;; therefore, to check them, 1) sort their attributes, 2) straight list comparison.
  ;; use letrec because `define-simple-check` wants an expression in <=6.2
  
  ;; `stringify-attr` is needed because comparing attr keys won't work if there are two attrs with same key.
  ;; so the whole attr is converted into a single string for sorting, which lets the attr value act as a tiebreaker.
  ;; it doesn't matter that this sort may not be correct (in the sense of a desirable ordering)
  ;; it just needs to be stable (e.g., a certain set of attrs will always sort the same way)
  (equal? (sort-attrs tx1) (sort-attrs tx2)))

(define+provide+safe (attrs-equal? x1 x2)
  ((or/c txexpr-attrs? txexpr?) (or/c txexpr-attrs? txexpr?) . -> . boolean?)
  (apply txexprs-equal? (map (λ (x) `(_ ,(if (txexpr-attrs? x) x (get-attrs x)))) (list x1 x2))))

(provide+safe check-txexprs-equal?)
(define-simple-check (check-txexprs-equal? tx1 tx2)
  (txexprs-equal? tx1 tx2))

(module+ test
  (check-txexprs-equal? '(p ((b "foo")(a "bar")) (span ((d "foo")(c "bar"))))
                        '(p ((a "bar")(b "foo")) (span ((c "bar")(d "foo")))))
  ;; two attrs with same key
  (check-txexprs-equal? '(p ((a "foo")(a "bar")))
                        '(p ((a "bar")(a "foo")))))