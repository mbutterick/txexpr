#lang racket/base
(require (for-syntax racket/base racket/syntax syntax/strip-context))

;; use a separate test file to avoid cycle in loading
(define-syntax (test-safe-and-unsafe stx)
  (syntax-case stx ()
    [(_ exprs ...)
     (with-syntax ([module-without-contracts (generate-temporary)]
                   [module-with-contracts (generate-temporary)]) 
       (replace-context stx #'(begin
                                (module module-without-contracts racket
                                  (require rackunit "main.rkt")
                                  (define-syntax (values->list stx)
                                    (syntax-case stx ()
                                      [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))
                                  exprs ...)
                                (require 'module-without-contracts)
                                (module module-with-contracts racket
                                  (require rackunit (submod "main.rkt" safe))
                                  (define-syntax (values->list stx)
                                    (syntax-case stx ()
                                      [(_ values-expr) #'(call-with-values (λ () values-expr) list)]))
                                  exprs ...)
                                (require 'module-with-contracts))))]))

(test-safe-and-unsafe
 
 (check-true (txexpr-tag? 'foo))
 (check-false (txexpr-tag? "foo"))
 (check-false (txexpr-tag? 3))
 
 (check-false (txexpr-tags? 'foo))
 (check-true (txexpr-tags? '(foo bar)))
 
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
 (check-true (txexpr-element? 2)) ; a valid-char, but not in v6.0 xml:xexpr? 
 (check-true (txexpr-element? 65)) ; a valid-char
 (check-false (txexpr-element? 0)) ; not a valid-char
 
 (check-true (txexpr-elements? '("p" "foo" "123")))
 (check-true (txexpr-elements? '("p" "foo" 123))) ; includes number
 (check-true (txexpr-elements? '(p "foo" "123"))) ; includes symbol
 (check-false (txexpr-elements? "foo")) ; not a list
 (check-false (txexpr-elements? '(((key "value")) "foo" "bar"))) ; includes attr
 (check-false (txexpr-elements? '("foo" "bar" ((key "value"))))) ; malformed
 
 (check-true (txexpr? '(p)))
 (check-true (txexpr? '(div 2)))
 (check-true (txexpr? '(div (div ((foo "bar")) 2))))
 (check-true (txexpr? '(p "foo" "bar")))
 (check-true (txexpr? '(p ((key "value")) "foo" "bar")))
 (check-true (txexpr? '(p 123))) ; content is a number
 (check-false (txexpr? "foo")) ; not a list with symbol
 (check-false (txexpr? '(p "foo" "bar" ((key "value"))))) ; malformed
 (check-false (txexpr? '("p" "foo" "bar"))) ; no name
 
 (check-not-exn (λ _ (validate-txexpr '(p))))
 (check-not-exn (λ _ (validate-txexpr '(p "foo" "bar"))))
 (check-not-exn (λ _ (validate-txexpr '(p ((key "value")) "foo" "bar"))))
 (check-not-exn (λ _ (validate-txexpr '(p 123)))) ; content is a valid-char
 (check-exn exn:fail? (λ _ (validate-txexpr "foo"))) ; not a list with symbol
 (check-exn exn:fail? (λ _ (validate-txexpr '(p "foo" "bar" ((key "value")))))) ; malformed
 (check-exn exn:fail? (λ _ (validate-txexpr '("p" "foo" "bar")))) ; no name
 (check-exn exn:fail? (λ _ (validate-txexpr '(root ((id "top")(class 42)))))) ; malformed attrs
 
 
 (check-txexprs-equal? (make-txexpr 'p) '(p))
 (check-txexprs-equal? (make-txexpr 'p '((key "value"))) '(p ((key "value"))))
 (check-txexprs-equal? (make-txexpr 'p null '("foo" "bar")) '(p "foo" "bar"))
 (check-txexprs-equal? (make-txexpr 'p '((key "value")) (list "foo" "bar")) 
                       '(p ((key "value")) "foo" "bar"))
 
 (check-txexprs-equal? (txexpr 'p) '(p))
 (check-txexprs-equal? (txexpr 'p '((key "value"))) '(p ((key "value"))))
 (check-txexprs-equal? (txexpr 'p null '("foo" "bar")) '(p "foo" "bar"))
 (check-txexprs-equal? (txexpr 'p '((key "value")) (list "foo" "bar")) 
                       '(p ((key "value")) "foo" "bar"))

 (check-txexprs-equal? (txexpr* 'p) '(p))
 (check-txexprs-equal? (txexpr* 'p '((key "value"))) '(p ((key "value"))))
 (check-txexprs-equal? (txexpr* 'p null "foo" "bar") '(p "foo" "bar"))
 (check-txexprs-equal? (txexpr* 'p '((key "value")) "foo" "bar") 
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
 
 (check-equal? (attrs->hash '((foo "bar"))) '#hasheq((foo . "bar")))
 (check-equal? (attrs->hash '((foo "bar") (foo "fraw"))) '#hasheq((foo . "bar")))
 (check-equal? (attrs->hash #:hash-style? #t '((foo "bar") (foo "fraw"))) '#hasheq((foo . "fraw")))
 (check-equal? (attrs->hash '((foo "bar")) '(foo "fraw")) '#hasheq((foo . "bar")))
 (check-equal? (attrs->hash '((foo "bar")) '(foo "fraw") 'foo "dog") '#hasheq((foo . "bar")))
 (check-exn exn:fail:contract? (λ _ (attrs->hash 'foo "bar" 'zam)))
 
 (check-equal? (apply set (hash->attrs '#hash((foo . "bar")(hee . "haw"))))
               (apply set '((foo "bar")(hee "haw"))))
 
 (check-equal? (attr-ref '(p ((foo "bar"))) 'foo) "bar")
 (check-exn exn:fail? (λ _ (attr-ref '(p ((foo "bar"))) 'zam)))
 (check-equal? (attr-ref '(p ((foo "bar"))) 'zam 42) 42)
 (check-equal? (attr-ref '(p ((foo "bar"))) 'zam (λ _ (* 6 7))) 42)
 (check-txexprs-equal? (attr-set '(p ((foo "bar"))) 'foo "fraw") '(p ((foo "fraw"))))
 (check-txexprs-equal? (attr-set* '(p ((foo "bar"))) 'foo "fraw") '(p ((foo "fraw"))))
 (check-true (let ([result (attr-set* '(p ((foo "bar"))) 'foo "fraw" 'zim 'zam)])
               (and (member '(foo "fraw") (get-attrs result))
                    (member '(zim "zam") (get-attrs result)) #t)))
 (check-txexprs-equal? (attr-join '(p ((foo "bar"))) 'foo "zam") '(p ((foo "bar zam"))))
 (check-true (let ([result (attr-join '(p ((foo "bar"))) 'zim "zam")])
               (and (member '(foo "bar") (get-attrs result))
                    (member '(zim "zam") (get-attrs result)) #t)))
 
 (check-true (attrs-have-key? '(p ((color "red")(shape "circle"))) 'color))
 (check-true (attrs-have-key? '(p ((color "red")(shape "circle"))) "color"))
 (check-false (attrs-have-key? '((color "red")(shape "circle")) 'nonexistent))
 
 (check-true (attrs-equal? '(p ((color "red")(shape "circle")))
                           '(foo ((color "red")(shape "circle")))))
 
 (check-false (attrs-equal? '(p ((color "red")(shape "circle")))
                            '(foo ((color "blue")(shape "circle")))))
 
 (check-true (attrs-equal? '(p ((color "red")(shape "circle")))
                           '(foo ((shape "circle")(color "red")))))
 
 (check-false (attrs-equal? '(p ((color "red")(shape "circle")))
                            '(foo ((color "red")))))
 
 (check-true (attrs-equal? '((color "red")(shape "circle"))
                           '((color "red")(shape "circle"))))
 
 (check-false (attrs-equal? '((color "red")(shape "circle"))
                            '((color "blue")(shape "circle"))))
 
 (check-true (attrs-equal? '((color "red")(shape "circle"))
                           '((shape "circle")(color "red"))))
 
 (check-false (attrs-equal? '((color "red")(shape "circle"))
                            '((color "red"))))
 
 (check-true (attrs-equal? '((foo "bar")(foo "zam")(zing "zong"))
                           '((foo "zam")(zing "zong")(foo "bar"))))
 (check-false (attrs-equal? '((foo "bar")(foo "zam")(zing "zong"))
                            '((foo "different")(zing "zong")(foo "bar"))))
 
 
 (define-simple-check (check-attrs-equal? attrs1 attrs2) (attrs-equal? attrs1 attrs2))
 
 (check-attrs-equal? '((foo "bar")(foo "zam")) '((foo "zam")(foo "bar")))
 
 (check-txexprs-equal? (remove-attrs '(p ((foo "bar")) "hi")) '(p "hi"))
 (check-txexprs-equal? (remove-attrs '(p ((foo "bar")) "hi" (p ((foo "bar")) "hi"))) '(p "hi" (p "hi")))
 
 
 (check-txexprs-equal? (map-elements (λ(x) (if (string? x) "boing" x))  
                                     '(p ((id "zam")) "foo" "bar" (em "square"))) 
                       '(p ((id "zam")) "boing" "boing" (em "boing")))
 
 (check-equal? (attr-set '(p) 'foo "zim") '(p ((foo "zim"))))
 (check-equal? (attr-set '(p ((foo "bar")(foo "zam"))) 'foo "zim") '(p ((foo "zim"))))
 
 (check-exn exn:fail:contract? (λ _ (attr-set* '(p) 'foo "bar" 'zam)))
 
 
 (define split-this-tx '(root (meta "foo" "bar") "hello" "world" (meta "foo2" "bar2") 
                              (em "goodnight" "moon" (meta "foo3" "bar3"))))
 (define split-predicate (λ(x) (and (txexpr? x) (eq? 'meta (get-tag x)))))
 (check-txexprs-equal? (call-with-values (λ() (splitf-txexpr split-this-tx split-predicate)) list) 
                       (list '(root "hello" "world" (em "goodnight" "moon")) '((meta "foo" "bar") (meta "foo2" "bar2") (meta "foo3" "bar3"))))
 
 (define split-proc (λ(x) '(div "foo")))
 (check-txexprs-equal? (call-with-values (λ() (splitf-txexpr split-this-tx split-predicate split-proc)) list) 
                       (list '(root (div "foo") "hello" "world" (div "foo") (em "goodnight" "moon" (div "foo"))) '((meta "foo" "bar") (meta "foo2" "bar2") (meta "foo3" "bar3"))))
 
 (define false-pred (λ(x) (and (txexpr? x) (eq? 'nonexistent-tag (get-tag x)))))
 (check-equal? (findf*-txexpr split-this-tx split-predicate) '((meta "foo" "bar") (meta "foo2" "bar2") (meta "foo3" "bar3")))
 (check-false (findf*-txexpr split-this-tx false-pred))
 (check-equal? (findf-txexpr split-this-tx split-predicate) '(meta "foo" "bar"))
 (check-false (findf-txexpr split-this-tx false-pred))
 
 (check-equal? (xexpr->html '(root (script "3 > 2") "Why is 3 > 2?"))
               "<root><script>3 > 2</script>Why is 3 &gt; 2?</root>"))