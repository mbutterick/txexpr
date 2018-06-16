#lang racket/base

(require rackunit "../stx.rkt" "check-values.rkt"
         (for-syntax racket/base))

;; Works on fully wrapped, non-wrapped, and partially
;; wrapped values, and it checks that the the inputs
;; are wrapped in all the same places. It checks scopes,
;; but it does not check source location.
(define-binary-check (check-stx=? stx=? actual expected))

(define (stx=? a b)
  (cond
    [(and (identifier? a) (identifier? b))
     (bound-identifier=? a b)]
    [(and (syntax? a) (syntax? b))
     (and (bound-identifier=? (datum->syntax a '||) (datum->syntax b '||))
          (stx=? (syntax-e a) (syntax-e b)))]
    [else
     (equal?/recur a b stx=?)]))

(define-syntax check-values-stx=?
  (lambda (stx)
    (syntax-case stx []
      [(_ actual-expr expected-expr)
       (syntax/loc stx
         (check/values check-stx=? actual-expr expected-expr))])))

;; ---------------------------------------------------------

;; Predicates

(check-pred stx-xexpr? "A leaf on a string")
(check-pred stx-xexpr? #'"A leaf in syntax")
(check-pred stx-xexpr? #'(div))
(check-pred stx-xexpr? #'(div ((id "top")) "Hello" (p "World")))
(check-pred stx-xexpr? `(div ((id ,#'"top")) "Hello" ,#'(p "World")))

(check-false (stx-txexpr? "A leaf without a tag"))
(check-pred stx-txexpr? '(div))
(check-pred stx-txexpr? #'(div))
(check-pred stx-txexpr? #'(div ((id "top")) "Hello" (p "World")))
(check-pred stx-txexpr? `(div ((id ,#'"top")) "Hello" ,#'(p "World")))

(check-pred stx-txexpr-tag? 'div)
(check-pred stx-txexpr-tag? #'div)
(check-pred stx-txexpr-tag? 'this-is-something-else)
(check-pred stx-txexpr-tag? #'this-is-something-else)

(check-pred stx-txexpr-attrs? '())
(check-pred stx-txexpr-attrs? #'())
(check-pred stx-txexpr-attrs? '((id "top") (stlye "color: blue")))
(check-pred stx-txexpr-attrs? #'((id "top") (stlye "color: blue")))
(check-pred stx-txexpr-attrs? `((id "top") (stlye ,#'"color: blue")))

;; ---------------------------------------------------------

;; Accessors

(check-values-stx=? (stx-txexpr->values '(p))
                    (values 'p null null))
(check-values-stx=? (stx-txexpr->values '(p "foo"))
                    (values 'p null '("foo")))
(check-values-stx=? (stx-txexpr->values '(p ((key "value"))))
                    (values 'p '((key "value")) null))
(check-values-stx=? (stx-txexpr->values '(p ((key "value")) "foo"))
                    (values 'p '((key "value")) '("foo")))

(check-values-stx=? (stx-txexpr->values #'(p))
                    (values #'p null null))
(check-values-stx=? (stx-txexpr->values #'(p "foo"))
                    (values #'p null (list #'"foo")))
(check-values-stx=? (stx-txexpr->values #'(p ((key "value"))))
                    (values #'p #'((key "value")) null))
(check-values-stx=? (stx-txexpr->values #'(p ((key "value")) "foo"))
                    (values #'p #'((key "value")) (list #'"foo")))

(check-values-stx=? (stx-txexpr->values `(,#'p))
                    (values #'p null null))
(check-values-stx=? (stx-txexpr->values `(p ,#'"foo"))
                    (values 'p null (list #'"foo")))
(check-values-stx=? (stx-txexpr->values `(p ((,#'key "value")) . ,#'("foo")))
                    (values 'p `((,#'key "value")) (list #'"foo")))


(check-stx=? (stx-txexpr-tag '(p ((key "value"))"foo" (em "square")))
             'p)
(check-stx=? (stx-txexpr-tag #'(p ((key "value"))"foo" (em "square")))
             #'p)

(check-stx=? (stx-txexpr-attrs '(p ((key "value"))"foo" "bar" (em "square")))
             '((key "value")))
(check-stx=? (stx-txexpr-attrs #'(p ((key "value"))"foo" "bar" (em "square")))
             #'((key "value")))
(check-stx=? (stx-txexpr-attrs '(p "foo" "bar" (em "square")))
             '())
(check-stx=? (stx-txexpr-attrs #'(p "foo" "bar" (em "square")))
             '())

(check-stx=? (stx-txexpr-elements '(p "foo" "bar" (em "square"))) 
             '("foo" "bar" (em "square")))
(check-stx=? (stx-txexpr-elements #'(p "foo" "bar" (em "square"))) 
             (list #'"foo" #'"bar" #'(em "square")))

(check-stx=? (stx-txexpr-elements '(p ((k "v"))"foo" "bar" (em "square"))) 
             '("foo" "bar" (em "square")))
(check-stx=? (stx-txexpr-elements #'(p ((k "v"))"foo" "bar" (em "square"))) 
             (list #'"foo" #'"bar" #'(em "square")))
(check-stx=? (stx-txexpr-elements #'(p ((k "v"))"foo" . ("bar" (em "square"))))
             (list #'"foo" #'"bar" #'(em "square")))
(check-stx=? (stx-txexpr-elements `(p ((k "v"))"foo" .,#'("bar" (em "square"))))
             (list "foo" #'"bar" #'(em "square")))

;; ---------------------------------------------------------

