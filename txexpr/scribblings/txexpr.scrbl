#lang scribble/manual

@; for documentation purposes, use the xexpr? from xml.
@; the one in txexpr is just to patch over an issue with
@; `valid-char?` in Racket 6.
@(require scribble/eval
          (for-label racket txexpr txexpr/stx xml rackunit))

@(define my-eval (make-base-eval))
@(my-eval `(require txexpr xml rackunit))


@title{txexpr: Tagged X-expressions}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodule[#:multi (txexpr (submod txexpr safe))]

A set of small but handy functions for improving the readability and reliability of programs that operate on tagged X-expressions (for short, @italic{txexpr}s).


@section{Installation}

At the command line:
@verbatim{raco pkg install txexpr}

After that, you can update the package from the command line:
@verbatim{raco pkg update txexpr}

@section{Importing the module}

The module can be invoked two ways: fast or safe. 

Fast mode is the default, which you get by importing the module in the usual way: @code{(require txexpr)}. 

Safe mode enables the function contracts documented below. Use safe mode by importing the module as @code{(require (submod txexpr safe))}.


@section[#:tag "what-is-a-txexpr"]{What’s a tagged X-expression (aka txexpr)?}

It's an X-expression with the following grammar:

@racketgrammar*[
#:literals (cons list symbol? string? xexpr?)
[txexpr (list tag (list attr ...) element ...)
      (cons tag (list element ...))]
[tag symbol?]
[attr (list key value)]
[key symbol?]
[value string?]
[element xexpr?]
]

A tagged X-expression — @italic{txexpr} for short — is a list with a symbol in the first position — the @italic{tag} — followed by a series of @italic{elements}, which are other X-expressions. Optionally, a txexpr can have a list of @italic{attributes} in the second position.

@examples[#:eval my-eval
(txexpr? '(span "Brennan" "Dale"))
(txexpr? '(span "Brennan" (em "Richard") "Dale"))
(txexpr? '(span ((class "hidden")(id "names")) "Brennan" "Dale"))
(txexpr? '(span lt gt amp))
(txexpr? '("We really" "should have" "a tag"))
(txexpr? '(span ((class not-quoted)) "Brennan")) 
(txexpr? '(span (class "hidden") "Brennan" "Dale")) 
   ]
   
The last one is a common mistake. Because the key–value pair is not enclosed in a @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{list}, it's interpreted as a nested txexpr within the first txexpr, as you may not find out until you try to read its attributes:

@examples[#:eval my-eval
(get-attrs '(span (class "hidden") "Brennan" "Dale")) 
(get-elements '(span (class "hidden") "Brennan" "Dale")) 
]

Tagged X-expressions are most commonly found in HTML & XML documents. Though the notation is different in Racket, the data structure is identical:

@examples[#:eval my-eval
(xexpr->string '(span ((id "names")) "Brennan" (em "Richard") "Dale"))
(string->xexpr "<span id=\"names\">Brennan<em>Richard</em>Dale</span>")
]

After converting to and from HTML, we get back the original X-expression. Well, almost. Per its usual practice, @racket[string->xexpr] added an empty attribute list after @racket[em]. This is benign — an empty attribute list can be omitted with no change in meaning, or vice versa.

@section{Why not just use @exec{match}, @exec{quasiquote}, and so on?}

If you prefer those, please do. But I've found two benefits to using module functions:

@bold{Readability.} In code that already has a lot of matching and quasiquoting going on, these functions make it easy to see where & how txexprs are being used.

@bold{Reliability.} Because txexprs come in two close but not quite equal forms, careful coders will always have to take both cases into account.

The programming is trivial, but the annoyance is real.

@section{Predicates}


@deftogether[(
@defproc[
(txexpr?
[v any/c])
boolean?]

@defproc[
(txexpr-tag?
[v any/c])
boolean?]

@defproc[
(txexpr-attr?
[v any/c])
boolean?]

@defproc[
(txexpr-attr-key?
[v any/c])
boolean?]

@defproc[
(txexpr-attr-value?
[v any/c])
boolean?]

@defproc[
(txexpr-element?
[v any/c])
boolean?]

)]
Predicates for @racket[_txexpr]s that implement this grammar:

@racketgrammar*[
#:literals (cons list symbol? string? xexpr?)
[txexpr (list tag (list attr ...) element ...)
      (cons tag (list element ...))]
[tag symbol?]
[attr (list key value)]
[key symbol?]
[value string?]
[element xexpr?]
]

@deftogether[(

@defproc[
(txexpr-tags?
[v any/c])
boolean?]

@defproc[
(txexpr-attrs?
[v any/c])
boolean?]

@defproc[
(txexpr-elements?
[v any/c])
boolean?]
)]
Predicates equivalent to a list of @code{txexpr-tag?}, @code{txexpr-attr?}, or @code{txexpr-element?}, respectively.



@deftogether[(

@defproc[
(can-be-txexpr-attr-key?
[v any/c])
boolean?]

@defproc[
(can-be-txexpr-attr-value?
[v any/c])
boolean?]

)]
Predicates for input arguments that can be trivially converted to an attribute @racket[_key] or @racket[_value] with the associated conversion functions.

@examples[#:eval my-eval
(can-be-txexpr-attr-key? 'symbol)
(can-be-txexpr-attr-key? "string-val")
(can-be-txexpr-attr-key? (list 1 2 3))
(can-be-txexpr-attr-value? 'symbol)
(can-be-txexpr-attr-value? "string-val")
(can-be-txexpr-attr-value? (list 1 2 3))
]



@defproc[
(can-be-txexpr-attrs?
[v any/c])
boolean?]
Predicate for functions that handle @racket[_txexpr-attrs]. Covers values that are easily converted into pairs of @racket[_attr-key] and @racket[_attr-value]. Namely: single @racket[_xexpr-attr]s, lists of @racket[_xexpr-attr]s (i.e., what you get from @racket[get-attrs]), or interleaved symbols and strings (each pair will be concatenated into a single @racket[_xexpr-attr]).


@defproc[
(validate-txexpr
[possible-txexpr any/c])
txexpr?]
Like @racket[txexpr?], but raise a descriptive error if @racket[_possible-txexpr] is invalid, and otherwise return @racket[_possible-txexpr] itself.

@examples[#:eval my-eval
(validate-txexpr 'root)
(validate-txexpr '(root))
(validate-txexpr '(root ((id "top")(class 42))))
(validate-txexpr '(root ((id "top")(class "42"))))
(validate-txexpr '(root ((id "top")(class "42")) ("hi")))
(validate-txexpr '(root ((id "top")(class "42")) "hi"))
]




@section{Making & breaking}

@defproc[
(txexpr
[tag txexpr-tag?] 
[attrs txexpr-attrs? @empty]
[elements txexpr-elements? @empty])
txexpr?]
Assemble a @racket[_txexpr] from its parts. If you don't have attributes, but you do have elements, you'll need to pass @racket[empty] (or @racket[null] or @racket['()]) as the second argument. Note that unlike @racket[xml->xexpr], if the attribute list is empty, it's not included in the resulting expression.

@examples[#:eval my-eval
(txexpr 'div)
(txexpr 'div '() '("Hello" (p "World")))
(txexpr 'div '((id "top")))
(txexpr 'div '((id "top")) '("Hello" (p "World")))
(define tx '(div ((id "top")) "Hello" (p "World")))
(txexpr (get-tag tx) 
(get-attrs tx) (get-elements tx))
]

@defproc[
(txexpr*
[tag txexpr-tag?] 
[attrs txexpr-attrs? @empty]
[element txexpr-element?] ...)
txexpr?]
Like @racket[txexpr], but takes an indefinite number of @racket[_element] arguments, which together are treated as the list of elements for the resulting @racket[_txexpr]. A notational convenience.

@examples[#:eval my-eval
(txexpr* 'div)
(txexpr* 'div '() "Hello" '(p "World"))
(txexpr* 'div '((id "top")))
(txexpr* 'div '((id "top")) "Hello" '(p "World"))
(define tx '(div ((id "top")) "Hello" (p "World")))
(apply txexpr* (get-tag tx) 
(get-attrs tx) (get-elements tx))
]


@deftogether[(
@defproc[
(get-tag
[tx txexpr?])
txexpr-tag?]

@defproc[
(get-attrs
[tx txexpr?])
txexpr-attr?]

@defproc[
(get-elements
[tx txexpr?])
(listof txexpr-element?)]
)]
Accessor functions for the individual pieces of a @racket[_txexpr].

@examples[#:eval my-eval
(get-tag '(div ((id "top")) "Hello" (p "World")))
(get-attrs '(div ((id "top")) "Hello" (p "World")))
(get-elements '(div ((id "top")) "Hello" (p "World")))
]


@deftogether[(

@defproc[
(txexpr->values
[tx txexpr?]) 
(values txexpr-tag? txexpr-attrs? txexpr-elements?)]

@defproc[
(txexpr->list
[tx txexpr?]) 
(list txexpr-tag? 
txexpr-attrs? 
txexpr-elements?)]
)]
Dissolve a @racket[_txexpr] into its components. @racket[txexpr->values] returns the components as multiple values; @racket[txexpr->list] returns them in a list.

@examples[#:eval my-eval
(txexpr->values '(div))
(txexpr->values '(div "Hello" (p "World")))
(txexpr->values '(div ((id "top")) "Hello" (p "World")))
(txexpr->list '(div))
(txexpr->list '(div "Hello" (p "World")))
(txexpr->list '(div ((id "top")) "Hello" (p "World")))
]

@section{Attributes}

@deftogether[(
@defproc[
(attrs->hash 
[#:hash-style? hash-style-priority boolean? #f]
[x can-be-txexpr-attrs?] ...)
hash-eq?]

@defproc[
(hash->attrs
[h hash?])
txexpr-attrs?]

)]
Convert @racket[_attrs] to an immutable hash, and back again. Following the convention specified for @link["https://www.w3.org/TR/xml/#attdecls"]{XML parsers}, the @italic{first} appearance of an attribute name binds the value — later attributes with the same name are ignored. If you prefer the typical @racket[hash] behavior where later values override earlier ones, set @racket[#:hash-style?] to @racket[#t].

@examples[#:eval my-eval
(define tx '(div ((id "top")(class "red")) "Hello" (p "World")))
(attrs->hash (get-attrs tx))
(hash->attrs '#hasheq((class . "red") (id . "top")))
(attrs->hash '((color "blue")(color "green")))
(attrs->hash #:hash-style? #t '((color "blue")(color "green")))
]

@defproc[
(attrs-have-key?
[attrs (or/c txexpr-attrs? txexpr?)]
[key can-be-txexpr-attr-key?])
boolean?]
Return @racket[#t] if the @racket[_attrs] contain a value for the given @racket[_key], @racket[#f] otherwise.

@examples[#:eval my-eval
(define tx '(div ((id "top")(class "red")) "Hello" (p "World")))
(attrs-have-key? tx 'id)
(attrs-have-key? tx 'grackle)
]

@defproc[
(attrs-equal?
[attrs (or/c txexpr-attrs? txexpr?)]
[other-attrs (or/c txexpr-attrs? txexpr?)])
boolean?]
Return @racket[#t] if @racket[_attrs] and @racket[_other-attrs] contain the same keys and values, @racket[#f] otherwise. The order of attributes is irrelevant. (If order matters to you, use good old @racket[equal?] instead.)

@examples[#:eval my-eval
(define tx1 '(div ((id "top")(class "red")) "Hello"))
(define tx2 '(p ((class "red")(id "top")) "Hello"))
(define tx3 '(p ((id "bottom")(class "red")) "Hello"))
(attrs-equal? tx1 tx2)
(attrs-equal? tx1 tx3)
(equal? tx1 tx2)
(equal? tx1 tx3)
]

@defproc[
(attr-ref
[tx txexpr?]
[key can-be-txexpr-attr-key?]
[failure-result any/c (λ _ (raise (make-exn:fail:contract ....)))
])
any]
Given a @racket[_key], return the corresponding @racket[_value] from the attributes of a @racket[_txexpr]. By default, asking for a nonexistent key produces an error. But if a value or procedure is provided as the @racket[_failure-result], evaluate and return that instead.

@examples[#:eval my-eval
(attr-ref tx 'class)
(attr-ref tx 'id)
(attr-ref tx 'nonexistent-key)
(attr-ref tx 'nonexistent-key "forty-two")
(attr-ref tx 'nonexistent-key (λ _ (* 6 7)))
]

@deftogether[(

@defproc[
(attr-set
[tx txexpr?]
[key can-be-txexpr-attr-key?]
[value can-be-txexpr-attr-value?])
txexpr?]


@defproc[
(attr-set*
[tx txexpr?]
[key can-be-txexpr-attr-key?]
[value can-be-txexpr-attr-value?] ... ... )
txexpr?]
)]
Set the value of attribute @racket[_key] to @racket[_value] in @racket[_txexpr]. Return the updated @racket[_txexpr]. Duplicate attributes, if they exist, are resolved using @racket[attrs->hash]. @racket[attr-set] only accepts one key and one value; @racket[attr-set*] accepts any number.

@examples[#:eval my-eval
(define tx '(div ((class "red")(id "top")) "Hello" (p "World")))
(attr-set tx 'id "bottom")
(attr-set tx 'class "blue")
(attr-set (attr-set tx 'id "bottom") 'class "blue")
(define tx '(div "Hello"))
(attr-set* tx 'id "bottom" 'class "blue")
]



@defproc[
(attr-join
[tx txexpr?]
[key can-be-txexpr-attr-key?]
[value can-be-txexpr-attr-value?])
txexpr?]
Given a @racket[_txexpr], append attribute @racket[_key] with @racket[_value]. Return the updated @racket[_txexpr]. If @racket[_key] doesn't already exist, then add a new attribute (i.e., behave like @racket[attr-set]).

@examples[#:eval my-eval
(define tx '(div ((class "red")) "Hello"))
(attr-join tx 'class "small")
(attr-join tx 'klass "small")
]


@defproc[
(remove-attrs
[tx txexpr?])
txexpr?]
Recursively remove all attributes from @racket[_tx].

@examples[#:eval my-eval
(define tx '(div ((id "top")) "Hello" (p ((id "lower")) "World")))
(remove-attrs tx)
]

@section{Strange magic}

@defproc[
(map-elements
[proc procedure?]
[tx txexpr?])
txexpr?]
Recursively apply @racket[_proc] to all elements, leaving tags and attributes alone. Using plain @racket[map] will only process elements at the top level of @racket[_tx]. Usually that's not what you want.

@examples[#:eval my-eval
(define tx '(div "Hello!" (p "Welcome to" (strong "Mars"))))
(define upcaser (λ(x) (if (string? x) (string-upcase x) x)))
(map upcaser tx)
(map-elements upcaser tx)
]

In practice, most @racket[_txexpr-element]s are strings. But it's unwise to pass string-only procedures to @racket[map-elements], because an @racket[_txexpr-element] can be any kind of @racket[xexpr?], and an @racket[xexpr?] is not necessarily a string.

@examples[#:eval my-eval
(define tx '(p "Welcome to" (strong "Mars" amp "Sons")))
(map-elements string-upcase tx)
(define upcaser (λ(x) (if (string? x) (string-upcase x) x)))
(map-elements upcaser tx)
]


@defproc[
(splitf-txexpr
[tx txexpr?]
[pred procedure?]
[replace-proc procedure? (λ(x) null)])
(values txexpr? (listof txexpr-element?))]
Recursively descend through @racket[_txexpr] and extract all elements that match @racket[_pred]. Returns two values: a @racket[_txexpr] with the matching elements removed, and the list of matching elements. Sort of esoteric, but I've needed it more than once, so here it is.

@examples[#:eval my-eval
(define tx '(div "Wonderful day" (meta "weather" "good") "for a walk"))
(define is-meta? (λ(x) (and (txexpr? x) (equal? 'meta (get-tag x)))))
(splitf-txexpr tx is-meta?)
]

Ordinarily, the result of the split operation is to remove the elements that match @racket[_pred]. But you can change this behavior with the optional @racket[_replace-proc] argument.

@examples[#:eval my-eval
(define tx '(div "Wonderful day" (meta "weather" "good") "for a walk"))
(define is-meta? (λ(x) (and (txexpr? x) (equal? 'meta (get-tag x)))))
(define replace-meta (λ(x) '(em "meta was here")))
(splitf-txexpr tx is-meta? replace-meta)
]


@deftogether[(

@defproc[
(findf-txexpr
[tx txexpr?]
[pred procedure?])
(or/c #f txexpr-element?)]

@defproc[
(findf*-txexpr
[tx txexpr?]
[pred procedure?])
(or/c #f (listof txexpr-element?))]

)]
Like @racket[splitf-txexpr], but only retrieve the elements that match @racket[_pred]. @racket[findf*-txexpr] retrieves all results; @racket[findf-txexpr] only the first. In both cases, if there are no matches, you get @racket[#f].

@examples[#:eval my-eval
(define tx '(div "Wonderful day" (meta "weather" "good")
                 "for a walk" (meta "dog" "Roxy")))
(define is-meta? (λ(x) (and (txexpr? x) (eq? 'meta (get-tag x)))))
(findf*-txexpr tx is-meta?)
(findf-txexpr tx is-meta?)
(define is-zimzam? (λ(x) (and (txexpr? x) (eq? 'zimzam (get-tag x)))))
(findf*-txexpr tx is-zimzam?)
(findf-txexpr tx is-zimzam?)
]



@section{HTML conversion}

@defproc[
(xexpr->html
[x xexpr?])
string?]
Convert @racket[_x] to an HTML string. Better than @racket[xexpr->string] because consistent with the HTML spec, it will skip the content of @code{script} or @code{style} blocks. For convenience, this function will take any X-expression, not just tagged X-expressions.

@examples[#:eval my-eval
(define tx '(root (script "3 > 2") "Why is 3 > 2?"))
(xexpr->string tx)
(xexpr->html tx)
(map xexpr->html (list "string" 'entity 65))
]


@section{Unit testing}

@defproc[
(check-txexprs-equal?
[tx1 txexpr?]
[tx2 txexpr?])
void?]
Designed to be used with @racketmodname[rackunit]. Check whether @racket[_tx1] and @racket[_tx2] are @racket[equal?] except for ordering of attributes (which ordinarily has no semantic significance). Return @racket[void] if so, otherwise raise a check failure.

@examples[#:eval my-eval
(define tx1 '(div ((attr-a "foo")(attr-z "bar"))))
(define tx2 '(div ((attr-z "bar")(attr-a "foo"))))
(parameterize ([current-check-handler (λ _ (display "not "))])
  (display "txexprs are ")
  (check-txexprs-equal? tx1 tx2)
  (displayln "equal"))
]

If ordering of attributes is relevant to your test, then just use @racket[check-equal?] as usual.

@examples[#:eval my-eval
(define tx1 '(div ((attr-a "foo")(attr-z "bar"))))
(define tx2 '(div ((attr-z "bar")(attr-a "foo"))))
(parameterize ([current-check-handler (λ _ (display "not "))])
  (display "txexprs are ")
  (check-equal? tx1 tx2)
  (displayln "equal"))
]


@section{Syntax Versions of X-expressions}

@(define stx-eval (make-base-eval))
@(stx-eval `(require txexpr txexpr/stx xml rackunit))

@defmodule[txexpr/stx]{
This module provides functions for destructuring TX-expressions
that might be wrapped in syntax objects.
}

@defproc[(stx-xexpr? [v any/c]) boolean?]{
A predicate for X-expressions that might be wrapped in syntax
(or have parts of them wrapped in syntax). It returns
@racket[#true] for values that would become normal X-expressions
with @racket[(syntax->datum (datum->syntax #f v))].

@examples[#:eval stx-eval
(stx-xexpr? "A leaf on the wind")
(stx-xexpr? #'"A leaf in a bin")
(stx-xexpr? '(div ((id "top")) "Hello" (p "World")))
(stx-xexpr? #'(div ((id "top")) "Hello" (p "World")))
(stx-xexpr? `(div ((id ,#'"top")) "Hello" ,#'(p "World")))
]}

@defproc[(stx-txexpr? [v any/c]) boolean?]{
A predicate for Tagged X-expressions that might be wrapped in
syntax. It returns @racket[#true] for values that become one
of these with @racket[(syntax->datum (datum->syntax #f v))]:

@racketgrammar*[
#:literals (list)
[txexpr (list tag attrs xexpr ...)
        (list tag xexpr ...)]
]

@examples[#:eval stx-eval
(stx-txexpr? "A block at the top")
(stx-txexpr? '(div ((id "top")) "A block beneath a" (p "tag")))
(stx-txexpr? #'(div ((id "top")) "A block beneath a" (p "tag")))
(stx-txexpr? #'(div "A block beneath a" (p "tag")))
]}

@deftogether[[
  @defproc[(stx-txexpr-tag? [v any/c]) boolean?]
  @defproc[(stx-txexpr-attrs? [v any/c]) boolean?]
]]{
Predicates for sub-parts of TX-expressions that might be wrapped
in syntax. There return @racket[#true] for values that become
@racket[txexpr-tag?]s or @racket[txexpr-attrs?]s when unwrapped
with @racket[(syntax->datum (datum->syntax #f v))].

@examples[#:eval stx-eval
(stx-txexpr-tag? 'div)
(stx-txexpr-tag? #'div)
(stx-txexpr-tag? 'analogous)
(stx-txexpr-tag? #'analogous)
(stx-txexpr-attrs? '())
(stx-txexpr-attrs? #'())
(stx-txexpr-attrs? '((id "top") (style "color: blue")))
(stx-txexpr-attrs? #'((id "top") (style "color: blue")))
(stx-txexpr-attrs? `((id "top") (style ,#'"color: blue")))
]}

@deftogether[[
  @defproc[(stx-txexpr-tag [tx stx-txexpr?]) stx-txexpr-tag?]
  @defproc[(stx-txexpr-attrs [tx stx-txexpr?]) stx-txexpr-attrs?]
  @defproc[(stx-txexpr-elements [tx stx-txexpr?]) (listof stx-txexpr?)]
]]{
Accessor functions for the tag, attributes, and elements of a
txexpr that might be wrapped in syntax. Note that these functions
work whether the input is wrapped in syntax or not, and that
the results may or may not be wrapped in syntax, depending on
whether the input was wrapped.

@examples[#:eval stx-eval
(define tx1 '(div ((id "top")) "Hello" (p "World")))
(define tx2 #'(div ((id "top")) "Hello" (p "World")))
(stx-txexpr-tag tx1)
(stx-txexpr-tag tx2)
(stx-txexpr-attrs tx1)
(stx-txexpr-attrs tx2)
(stx-txexpr-elements tx1)
(stx-txexpr-elements tx2)
]}

@deftogether[[
  @defproc[(stx-txexpr->values [tx stx-txexpr?])
           (values stx-txexpr-tag? stx-txexpr-attrs? (listof stx-txexpr?))]
  @defproc[(stx-txexpr->list [tx stx-txexpr?])
           (list/c stx-txexpr-tag? stx-txexpr-attrs? (listof stx-txexpr?))]
]]{
These functions break up a TX-expression into its components.
@racket[stx-txexpr->values] returns them as three values, and
@racket[stx-txexpr->list] returns them as a three-element list.

@examples[#:eval stx-eval
(stx-txexpr->values '(div))
(stx-txexpr->list '(div))
(stx-txexpr->values #'(div))
(stx-txexpr->values #'(div "Hello" (p "World")))
(stx-txexpr->values #'(div ((id "top")) "Hello" (p "World")))
(stx-txexpr->values `(div ((id "top")) "Hello" ,#'(p "World")))
]}

@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/txexpr"]{http://github.com/mbutterick/txexpr}. Suggestions & corrections welcome.

