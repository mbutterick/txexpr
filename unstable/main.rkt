#lang racket/base
(require "../main.rkt" sugar/define)

(module+ test
  (require rackunit "../main.rkt"))

(define+provide+safe (attr-ref* tx key)
  (txexpr? can-be-txexpr-attr-key? . -> . txexpr-attr-values?)
  (define results empty)
  (let loop ([tx tx])
    (when (and (txexpr? tx) (attrs-have-key? tx key) (attr-ref tx key))
      (set! results (cons (attr-ref tx key) results))
      (map loop (get-elements tx))
      (void)))
  (reverse results))

(module+ test
  (check-txexprs-equal? (attr-ref* '(root ((foo "bar")) "hello" "world" (meta ((foo "zam")) "bar2") 
                                 (em ((foo "zam")) "goodnight" "moon")) 'foo) '("bar" "zam" "zam"))
 
 (check-txexprs-equal? (attr-ref* '(root ((foo "bar")) "hello" "world" (meta ((foo "zam")) "bar2") 
                                 (em ((foo "zam")) "goodnight" "moon")) 'nonexistent-key) '()))


;; convert list of alternating keys & values to attr
;; with override behavior (using hash)
(define+provide+safe (merge-attrs . items)
  (() #:rest list-of-can-be-txexpr-attrs? . ->* . txexpr-attrs?)
  (hash->attrs (apply (λ xs (attrs->hash #:hash-style? #t xs)) items)))

(module+ test
  (check-true (attrs-equal? (merge-attrs 'foo "bar") '((foo "bar"))))
  (check-true (attrs-equal? (merge-attrs '(foo "bar")) '((foo "bar"))))
  (check-true (attrs-equal? (merge-attrs '((foo "bar"))) '((foo "bar"))))
  (check-true (attrs-equal? (merge-attrs "foo" 'bar) '((foo "bar"))))
  (check-true (attrs-equal? (merge-attrs "foo" "bar" "goo" "gar") '((foo "bar")(goo "gar"))))
  (check-true (attrs-equal? (merge-attrs (merge-attrs "foo" "bar" "goo" "gar") "hee" "haw") 
                      '((foo "bar")(goo "gar")(hee "haw"))))
  (check-true (attrs-equal? (merge-attrs '((foo "bar")(goo "gar")) "foo" "haw") '((foo "haw")(goo "gar")))))