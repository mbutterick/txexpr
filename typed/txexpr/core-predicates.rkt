#lang typed/racket/base
(require (for-syntax racket/base) racket/match typed/sugar/define)
(provide (all-defined-out))

; Section 2.2 of XML 1.1
; (XML 1.0 is slightly different and more restrictive)
(define/typed (valid-char? i)
  (Any -> Boolean)
  (and (exact-nonnegative-integer? i)
       (or (<= #x1     i #xD7FF)
           (<= #xE000  i #xFFFD)
           (<= #x10000 i #x10FFFF))))

(require/typed
 xml
 [#:struct location ([line : (Option Natural)]
                     [char : (Option Natural)]
                     [offset : Natural])]
 [#:struct source ([start : (U location Symbol #f)]
                   [stop : (U location Symbol #f)])]
 [#:struct (cdata source) ([string : String])]
 [#:struct comment ([text : String])]
 [#:struct (p-i source) ([target-name : Symbol]
                         [instruction : String])]
 [xexpr->string (Xexpr -> String)])
(provide (all-from-out xml) cdata? xexpr->string)


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

(define-predicate xexpr? Xexpr)
(define-predicate txexpr? Txexpr)
(define-predicate txexpr-short? Txexpr-Short)
(define-predicate txexpr-tag? Txexpr-Tag)
(define-predicate txexpr-tags? (Listof Txexpr-Tag))
(define-predicate txexpr-attr? Txexpr-Attr)
(define-predicate txexpr-attrs? Txexpr-Attrs)
(define-predicate Valid-Char? Valid-Char)
(define/typed (txexpr-element? x)
  (Any -> Boolean)
  (if (xexpr? x)
      (if (Valid-Char? x) (valid-char? x) #t)
      #f))
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