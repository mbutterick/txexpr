#lang racket/base
(require "main.rkt" rackunit)
(define empty '())

;; helper for comparison of values
;; normal function won't work for this. Has to be syntax-rule
(define-syntax-rule (values->list vs)
  (call-with-values (λ() vs) list))

(check-true (xexpr-attr? '()))
(check-true (xexpr-attr? '((key "value"))))
(check-true (xexpr-attr? '((key "value") (foo "bar"))))
(check-false (xexpr-attr? '((key "value") "foo" "bar"))) ; content, not attr
(check-false (xexpr-attr? '(key "value"))) ; not a nested list
(check-false (xexpr-attr? '(("key" "value")))) ; two strings
(check-false (xexpr-attr? '((key value)))) ; two symbols

(check-true (xexpr-elements? '("p" "foo" "123")))
(check-true (xexpr-elements? '("p" "foo" 123))) ; includes number
(check-true (xexpr-elements? '(p "foo" "123"))) ; includes symbol
(check-false (xexpr-elements? "foo")) ; not a list
(check-false (xexpr-elements? '(((key "value")) "foo" "bar"))) ; includes attr
(check-false (xexpr-elements? '("foo" "bar" ((key "value"))))) ; malformed


(check-true (tagged-xexpr? '(p "foo" "bar")))
(check-true (tagged-xexpr? '(p ((key "value")) "foo" "bar")))
(check-true (tagged-xexpr? '(p 123))) ; content is a number
(check-false (tagged-xexpr? "foo")) ; not a list with symbol
(check-false (tagged-xexpr? '(p "foo" "bar" ((key "value"))))) ; malformed
(check-false (tagged-xexpr? '("p" "foo" "bar"))) ; no name


(check-equal? (make-xexpr-attr 'foo "bar") '((foo "bar")))
(check-equal? (make-xexpr-attr "foo" 'bar) '((foo "bar")))
(check-equal? (make-xexpr-attr "foo" "bar" "goo" "gar") '((foo "bar")(goo "gar")))
(check-equal? (make-xexpr-attr (make-xexpr-attr "foo" "bar" "goo" "gar") "hee" "haw") 
              '((foo "bar")(goo "gar")(hee "haw")))
(check-equal? (make-xexpr-attr '((foo "bar")(goo "gar")) "foo" "haw") '((foo "haw")(goo "gar")))


(check-equal? (make-tagged-xexpr 'p) '(p))
(check-equal? (make-tagged-xexpr 'p '((key "value"))) '(p ((key "value"))))
(check-equal? (make-tagged-xexpr 'p empty '("foo" "bar")) '(p "foo" "bar"))
(check-equal? (make-tagged-xexpr 'p '((key "value")) (list "foo" "bar")) 
              '(p ((key "value")) "foo" "bar"))

(check-equal? (values->list (break-tagged-xexpr '(p))) 
              (values->list (values 'p empty empty)))
(check-equal? (values->list (break-tagged-xexpr '(p "foo"))) 
              (values->list (values 'p empty '("foo"))))
(check-equal? (values->list (break-tagged-xexpr '(p ((key "value"))))) 
              (values->list (values 'p '((key "value")) empty)))
(check-equal? (values->list (break-tagged-xexpr '(p ((key "value")) "foo"))) 
              (values->list (values 'p '((key "value")) '("foo"))))


(check-equal? (tagged-xexpr-tag '(p ((key "value"))"foo" "bar" (em "square"))) 'p)
(check-equal? (tagged-xexpr-attr '(p ((key "value"))"foo" "bar" (em "square"))) '((key "value")))
(check-equal? (tagged-xexpr-elements '(p ((key "value"))"foo" "bar" (em "square"))) 
              '("foo" "bar" (em "square")))

(check-equal? (remove-attrs '(p ((foo "bar")) "hi")) '(p "hi"))
(check-equal? (remove-attrs '(p ((foo "bar")) "hi" (p ((foo "bar")) "hi"))) '(p "hi" (p "hi")))

(check-equal? (map-xexpr-elements (λ(x) (if (string? x) "boing" x))  
                                    '(p "foo" "bar" (em "square"))) 
                '(p "boing" "boing" (em "square")))

(define xx '(root (meta "foo" "bar") "hello" "world" (meta "foo2" "bar2") 
                    (em "goodnight" "moon" (meta "foo3" "bar3"))))
(check-equal? (values->list (split-tag-from-xexpr 'meta xx)) 
                (list '((meta "foo" "bar") (meta "foo2" "bar2") (meta "foo3" "bar3")) 
                      '(root "hello" "world" (em "goodnight" "moon"))))