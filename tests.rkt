#lang racket/base
(require (for-syntax racket/base))
(require (submod "main.rkt" safe) rackunit)

(define-syntax (values->list stx)
  (syntax-case stx ()
    [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))

(check-true (txexpr-tag?'foo))
(check-false (txexpr-tag? "foo"))
(check-false (txexpr-tag? 3))

(check-true (txexpr-attr? '(key "value")))
(check-false (txexpr-attr? '(key "value" "another")))
(check-false (txexpr-attr? '(key 0 "value")))

(check-true (txexpr-attrs? '()))
(check-true (txexpr-attrs? '((key "value"))))
(check-true (txexpr-attrs? '((key "value") (foo "bar"))))
(check-false (txexpr-attrs? '((key "value") "foo" "bar"))) ; content, not attr
(check-false (txexpr-attrs? '(key "value"))) ; not a nested list
(check-false (txexpr-attrs? '(("key" "value")))) ; two strings
(check-false (txexpr-attrs? '((key value)))) ; two symbols

(check-true (txexpr-element? "string"))
(check-true (txexpr-element? 'amp))
(check-true (txexpr-element? '(p "string")))
(check-true (txexpr-element? 65)) ;; a valid-char
(check-false (txexpr-element? 0)) ;; not a valid-char

(check-true (txexpr-elements? '("p" "foo" "123")))
(check-true (txexpr-elements? '("p" "foo" 123))) ; includes number
(check-true (txexpr-elements? '(p "foo" "123"))) ; includes symbol
(check-false (txexpr-elements? "foo")) ; not a list
(check-false (txexpr-elements? '(((key "value")) "foo" "bar"))) ; includes attr
(check-false (txexpr-elements? '("foo" "bar" ((key "value"))))) ; malformed

(check-true (txexpr? '(p)))
(check-true (txexpr? '(p "foo" "bar")))
(check-true (txexpr? '(p ((key "value")) "foo" "bar")))
(check-true (txexpr? '(p 123))) ; content is a number
(check-false (txexpr? "foo")) ; not a list with symbol
(check-false (txexpr? '(p "foo" "bar" ((key "value"))))) ; malformed
(check-false (txexpr? '("p" "foo" "bar"))) ; no name

(check-equal? (make-txexpr 'p) '(p))
(check-equal? (make-txexpr 'p '((key "value"))) '(p ((key "value"))))
(check-equal? (make-txexpr 'p null '("foo" "bar")) '(p "foo" "bar"))
(check-equal? (make-txexpr 'p '((key "value")) (list "foo" "bar")) 
              '(p ((key "value")) "foo" "bar"))

(check-equal? (values->list (txexpr->values '(p))) 
              (values->list (values 'p null null)))
(check-equal? (values->list (txexpr->values '(p "foo"))) 
              (values->list (values 'p null '("foo"))))
(check-equal? (values->list (txexpr->values '(p ((key "value"))))) 
              (values->list (values 'p '((key "value")) null)))
(check-equal? (values->list (txexpr->values '(p ((key "value")) "foo"))) 
              (values->list (values 'p '((key "value")) '("foo"))))

(check-equal? (values->list (txexpr->values '(p))) 
              (txexpr->list '(p)))
(check-equal? (values->list (txexpr->values '(p "foo"))) 
              (txexpr->list '(p "foo")))
(check-equal? (values->list (txexpr->values '(p ((key "value"))))) 
              (txexpr->list '(p ((key "value")))))
(check-equal? (values->list (txexpr->values '(p ((key "value")) "foo"))) 
              (txexpr->list '(p ((key "value")) "foo")))

(check-equal? (get-tag '(p ((key "value"))"foo" "bar" (em "square"))) 'p)
(check-equal? (get-attrs '(p ((key "value"))"foo" "bar" (em "square"))) '((key "value")))
(check-equal? (get-elements '(p ((key "value"))"foo" "bar" (em "square"))) 
              '("foo" "bar" (em "square")))


(check-equal? (->txexpr-attr-key "foo") 'foo)
(check-equal? (->txexpr-attr-key 'foo) 'foo)

(check-equal? (->txexpr-attr-value "foo") "foo")
(check-equal? (->txexpr-attr-value 'foo) "foo")

(check-equal? (attrs->hash '((foo "bar"))) '#hash((foo . "bar")))
(check-equal? (attrs->hash '((foo "bar")) '(foo "fraw")) '#hash((foo . "fraw")))
(check-equal? (attrs->hash '((foo "bar")) '(foo "fraw") 'foo "dog") '#hash((foo . "dog")))

(check-equal? (hash->attrs '#hash((foo . "bar")(hee . "haw"))) '((foo "bar")(hee "haw")))

(check-equal? (attr-ref '(p ((foo "bar"))) 'foo) "bar")
(check-equal? (attr-set '(p ((foo "bar"))) 'foo "fraw") '(p ((foo "fraw"))))

(check-true (attrs-have-key? '(p ((color "red")(shape "circle"))) 'color))
(check-true (attrs-have-key? '(p ((color "red")(shape "circle"))) "color"))
(check-false (attrs-have-key? '((color "red")(shape "circle")) 'nonexistent))


(check-equal? (merge-attrs 'foo "bar") '((foo "bar")))
(check-equal? (merge-attrs '(foo "bar")) '((foo "bar")))
(check-equal? (merge-attrs '((foo "bar"))) '((foo "bar")))
(check-equal? (merge-attrs "foo" 'bar) '((foo "bar")))
(check-equal? (merge-attrs "foo" "bar" "goo" "gar") '((foo "bar")(goo "gar")))
(check-equal? (merge-attrs (merge-attrs "foo" "bar" "goo" "gar") "hee" "haw") 
              '((foo "bar")(goo "gar")(hee "haw")))
(check-equal? (merge-attrs '((foo "bar")(goo "gar")) "foo" "haw") '((foo "haw")(goo "gar")))


(check-equal? (remove-attrs '(p ((foo "bar")) "hi")) '(p "hi"))
(check-equal? (remove-attrs '(p ((foo "bar")) "hi" (p ((foo "bar")) "hi"))) '(p "hi" (p "hi")))


(check-equal? (map-elements (λ(x) (if (string? x) "boing" x))  
                            '(p "foo" "bar" (em "square"))) 
              '(p "boing" "boing" (em "boing")))



(define split-this-tx '(root (meta "foo" "bar") "hello" "world" (meta "foo2" "bar2") 
                  (em "goodnight" "moon" (meta "foo3" "bar3"))))
(check-equal? (call-with-values (λ() (splitf-txexpr split-this-tx (λ(x) (and (txexpr? x) (equal? 'meta (car x)))))) list) 
              (list '(root "hello" "world" (em "goodnight" "moon")) '((meta "foo" "bar") (meta "foo2" "bar2") (meta "foo3" "bar3"))))

(check-equal? (xexpr->html '(root (script "3 > 2") "Why is 3 > 2?"))
              "<root><script>3 > 2</script>Why is 3 &gt; 2?</root>")