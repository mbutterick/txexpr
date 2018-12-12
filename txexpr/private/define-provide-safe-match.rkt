#lang racket/base

(provide define+provide+safe+match)

(require racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     version/utils))

;; (define+provide+safe+match name-id
;;   contract-expr
;;   value-expr
;;   #:match-expander
;;   match-transformer-expr)
;; 
;; (define+provide+safe+match (head . args)
;;   contract-expr
;;   value-body-expr
;;   ...+
;;   #:match-expander
;;   match-transformer-expr)

(begin-for-syntax
  ;; Identifier -> [Syntax -> Syntax]
  (define ((variable-like-transformer id) stx)
    (cond
      [(identifier? stx)
       ; id, but with the source location of stx
       (datum->syntax id (syntax-e id) stx id)]
      [(stx-pair? stx)
       (datum->syntax stx (cons id (stx-cdr stx)) stx stx)])))

(define-syntax define+provide+safe+match
  (λ (stx)
    (syntax-parse stx

      [(d (head . args)
          contract:expr
          value-body:expr ...+
          #:match-expander match-transformer:expr)
       #:with fn-expr (syntax/loc stx (λ args value-body ...))
       (syntax/loc stx
         (d head contract fn-expr #:match-expander match-transformer))]

      [(_ name:id
          contract:expr
          value:expr
          #:match-expander match-transformer:expr)
       #:with internal-name (generate-temporary #'name)
       #:with contract-name (generate-temporary #'name)
       #:with make-name-match-transformer (generate-temporary #'name)
       #:with [name-for-blame ...]
       (cond [(version<=? "6.8" (version)) #'[#:name-for-blame name]]
             [else                         #'[]])

       #'(begin
           (define internal-name (let ([name value]) name))

           (begin-for-syntax
             (define (make-name-match-transformer name)
               (with-syntax ([name name]) match-transformer)))

           (define-match-expander name
             (make-name-match-transformer (quote-syntax internal-name))
             (variable-like-transformer (quote-syntax internal-name)))

           (provide name)

           (module+ safe
             (require racket/contract/base)

             (define-module-boundary-contract contract-name internal-name contract
               name-for-blame ...)

             (define-match-expander name
               (make-name-match-transformer (quote-syntax contract-name))
               (variable-like-transformer (quote-syntax contract-name)))

             (provide name)))])))

