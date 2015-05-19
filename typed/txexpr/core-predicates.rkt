#lang typed/racket/base
(require (for-syntax racket/base) racket/match)
(provide (all-defined-out) valid-char? cdata?)

(require/typed
 xml
 [valid-char? (Any -> Boolean)]
 [#:struct location ([line : (Option Natural)]
                     [char : (Option Natural)]
                     [offset : Natural])]
 [#:struct source ([start : (U location Symbol #f)]
                   [stop : (U location Symbol #f)])]
 [#:struct (cdata source) ([string : String])]
 [#:struct comment ([text : String])]
 [#:struct (p-i source) ([target-name : Symbol]
                         [instruction : String])])

(define-type Valid-Char Natural) ;; overinclusive but that's as good as it gets
(define-type Txexpr-Tag Symbol)
(define-type Txexpr-Attr-Key Symbol)
(define-type Txexpr-Attr-Value String)
(define-type Txexpr-Attr (Pairof Txexpr-Attr-Key (Pairof Txexpr-Attr-Value Null)))
(define-predicate Txexpr-Attr? Txexpr-Attr)
(define-type Can-Be-Txexpr-Attr-Key (U Symbol String))
(define-type Can-Be-Txexpr-Attr-Value (U Symbol String))
(define-type Txexpr-Attrs (Listof Txexpr-Attr))
(define-type Txexpr-Attr-Hash (HashTable Txexpr-Attr-Key Txexpr-Attr-Value))
(define-type Txexpr-Element Xexpr)
(define-type Txexpr-Elements (Listof Txexpr-Element))
(define-type Txexpr-Full (List* Txexpr-Tag Txexpr-Attrs (Listof Xexpr)))
(define-type Txexpr-Short (Pairof Txexpr-Tag (Listof Xexpr)))
(define-type Txexpr (U Txexpr-Full Txexpr-Short))
(define-type Xexpr
  (U String
     Txexpr-Full
     Txexpr-Short
     Symbol
     Valid-Char
     cdata
     comment
     p-i))

(define-syntax (define/typed stx)
  (syntax-case stx ()
    [(_ (proc-name arg ... . rest-arg) type-expr body ...)
     #'(define/typed proc-name type-expr
         (λ(arg ... . rest-arg) body ...))]
    [(_ proc-name type-expr body ...)
     #'(begin
         (: proc-name type-expr)
         (define proc-name body ...))]))

(define-predicate txexpr? Txexpr)
(define-predicate txexpr-tag? Txexpr-Tag)
(define-predicate txexpr-tags? (Listof Txexpr-Tag))
(define-predicate txexpr-attr? Txexpr-Attr)
(define-predicate txexpr-attrs? Txexpr-Attrs)
(define-predicate txexpr-element? Xexpr)
(define-predicate txexpr-elements? (Listof Xexpr))
(define-predicate txexpr-attr-key? Txexpr-Attr-Key)
(define-predicate txexpr-attr-value? Txexpr-Attr-Value)
(define-predicate txexpr-attr-values? (Listof Txexpr-Attr-Value))
(define-predicate can-be-txexpr-attr-key? Can-Be-Txexpr-Attr-Key)
(define-predicate can-be-txexpr-attr-value? Can-Be-Txexpr-Attr-Value)
(define-predicate can-be-txexpr-attr? (List Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value))
(define-type Can-Be-Txexpr-Attr (U Txexpr-Attr Txexpr-Attrs Can-Be-Txexpr-Attr-Key Can-Be-Txexpr-Attr-Value))
(define-predicate can-be-txexpr-attrs? Can-Be-Txexpr-Attr)
(define-predicate list-of-can-be-txexpr-attrs? (Listof Can-Be-Txexpr-Attr))
