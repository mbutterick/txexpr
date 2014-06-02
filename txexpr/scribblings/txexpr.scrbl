#lang scribble/manual

@(require scribble/eval (for-label racket txexpr xml))

@(define my-eval (make-base-eval))
@(my-eval `(require txexpr xml))


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

The module operates in two modes: fast and safe. Fast mode is the default, which you get by importing the module in the usual way: @code{(require txexpr)}. 

Safe mode enables the function contracts documented below. Use safe mode by importing the module as @code{(require (submod txexpr safe))}.


@section{What’s a txexpr?}

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

A txexpr is a list with a symbol in the first position — the @italic{tag} — followed by a series of @italic{elements}, which are other X-expressions. Optionally, a txexpr can have a list of @italic{attributes} in the second position.

@examples[#:eval my-eval
(txexpr? '(span "Brennan" "Dale"))
(txexpr? '(span "Brennan" (em "Richard") "Dale"))
(txexpr? '(span [[class "hidden"][id "names"]] "Brennan" "Dale"))
(txexpr? '(span lt gt amp))
(txexpr? '("We really" "should have" "a tag"))
(txexpr? '(span [[class not-quoted]] "Brennan")) 
(txexpr? '(span [class "hidden"] "Brennan" "Dale")) 
   ]
   
The last one is a common mistake. Because the key–value pair is not enclosed in a @racket[list], it's interpreted as a nested txexpr within the first txexpr, as you may not find out until you try to read its attributes:

@margin-note{There's no way of eliminating this ambiguity, short of always requiring an attribute list — empty if necessary — in your txexpr. See also @racket[xexpr-drop-empty-attributes].}

@examples[#:eval my-eval
(get-attrs '(span [class "hidden"] "Brennan" "Dale")) 
(get-elements '(span [class "hidden"] "Brennan" "Dale")) 
]

Tagged X-expressions are most commonly found in HTML & XML documents. Though the notation is different in Racket, the data structure is identical:

@examples[#:eval my-eval
(xexpr->string '(span [[id "names"]] "Brennan" (em "Richard") "Dale"))
(string->xexpr "<span id=\"names\">Brennan<em>Richard</em>Dale</span>")
]

After converting to and from HTML, we get back the original X-expression. Well, almost. The brackets turned into parentheses — no big deal, since they mean the same thing in Racket. Also, per its usual practice, @racket[string->xexpr] added an empty attribute list after @racket[em]. This is also benign.

@section{Why not just use @exec{match}, @exec{quasiquote}, and so on?}

If you prefer those, please do. But I've found two benefits to using module functions:

@bold{Readability.} In code that already has a lot of matching and quasiquoting going on, these functions make it easy to see where & how txexprs are being used.

@bold{Reliability.} Because txexprs come in two close but not quite equal forms, careful coders will always have to take both cases into account.

The programming is trivial, but the annoyance is real.

@section{Interface}

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
(txexpr-attrs?
[v any/c])
boolean?]

@defproc[
(txexpr-elements?
[v any/c])
boolean?]
)]
Shorthand for @code{(listof txexpr-attr?)} and @code{(listof txexpr-element?)}.



@defproc[
(validate-txexpr
[possible-txexpr any/c])
txexpr?]
Like @racket[txexpr?], but raises a descriptive error if @racket[_possible-txexpr] is invalid, and otherwise returns @racket[_possible-txexpr] itself.

@examples[#:eval my-eval
(validate-txexpr 'root)
(validate-txexpr '(root))
(validate-txexpr '(root ((id "top")(class 42))))
(validate-txexpr '(root ((id "top")(class "42"))))
(validate-txexpr '(root ((id "top")(class "42")) ("hi")))
(validate-txexpr '(root ((id "top")(class "42")) "hi"))
]



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
Predicates for input arguments that are trivially converted to an attribute @racket[_key] or @racket[_value]…


@deftogether[(

@defproc[
(->txexpr-attr-key
[v can-be-txexpr-attr-key?])
txexpr-attr-key?]

@defproc[
(->txexpr-attr-value
[v can-be-txexpr-attr-value?])
txexpr-attr-value?]
)]
… with these conversion functions.


@defproc[
(txexpr->values
[tx txexpr?]) 
(values txexpr-tag? txexpr-attrs? txexpr-elements?)]
Dissolves a @racket[_txexpr] into its components and returns all three.

@examples[#:eval my-eval
(txexpr->values '(div))
(txexpr->values '(div "Hello" (p "World")))
(txexpr->values '(div [[id "top"]] "Hello" (p "World")))

]

@defproc[
(txexpr->list
[tx txexpr?]) 
(list txexpr-tag? 
txexpr-attrs? 
txexpr-elements?)]
Like @racket[txexpr->values], but returns the three components in a list.

@examples[#:eval my-eval
(txexpr->list '(div))
(txexpr->list '(div "Hello" (p "World")))
(txexpr->list '(div [[id "top"]] "Hello" (p "World")))
]

@defproc[
(xexpr->html
[x xexpr?])
string?]
Convert @racket[_x] to an HTML string. Better than @racket[xexpr->string] because consistent with the HTML spec, it will not escape text that appears within @code{script} or @code{style} blocks. For convenience, this function will take any X-expression, not just tagged X-expressions.

@examples[#:eval my-eval
(define tx '(root (script "3 > 2") "Why is 3 > 2?"))
(xexpr->string tx)
(xexpr->html tx)
(map xexpr->html (list "string" 'entity 65))
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
(get-tag '(div [[id "top"]] "Hello" (p "World")))
(get-attrs '(div [[id "top"]] "Hello" (p "World")))
(get-elements '(div [[id "top"]] "Hello" (p "World")))
]

@defproc[
(make-txexpr
[tag txexpr-tag?] 
[attrs txexpr-attrs? @empty]
[elements txexpr-elements? @empty])
txexpr?]
Assemble a @racket[_txexpr] from its parts. If you don't have attributes, but you do have elements, you'll need to pass @racket[empty] as the second argument. Note that unlike @racket[xml->xexpr], if the attribute list is empty, it's not included in the resulting expression.

@examples[#:eval my-eval
(make-txexpr 'div)
(make-txexpr 'div '() '("Hello" (p "World")))
(make-txexpr 'div '[[id "top"]])
(make-txexpr 'div '[[id "top"]] '("Hello" (p "World")))
(define tx '(div [[id "top"]] "Hello" (p "World")))
(make-txexpr (get-tag tx) 
(get-attrs tx) (get-elements tx))
]

@defproc[
(can-be-txexpr-attrs?
[v any/c])
boolean?]
Predicate for functions that handle @racket[_txexpr-attrs]. Covers values that are easily converted into pairs of @racket[_attr-key] and @racket[_attr-value]. Namely: single @racket[_xexpr-attr]s, lists of @racket[_xexpr-attr]s (i.e., what you get from @racket[get-attrs]), or interleaved symbols and strings (each pair will be concatenated into a single @racket[_xexpr-attr]).

@deftogether[(
@defproc[
(attrs->hash [x can-be-txexpr-attrs?] ...)
hash?]

@defproc[
(hash->attrs
[h hash?])
txexpr-attrs?]

)]
Convert @racket[_attrs] to an immutable hash, and back again.

@examples[#:eval my-eval
(define tx '(div [[id "top"][class "red"]] "Hello" (p "World")))
(attrs->hash (get-attrs tx))
(hash->attrs '#hash((class . "red") (id . "top")))
]

@defproc[
(attrs-have-key?
[attrs (or/c txexpr-attrs? txexpr?)]
[key can-be-txexpr-attr-key?])
boolean?]
Returns @racket[#t] if the @racket[_attrs] contain a value for the given @racket[_key], @racket[#f] otherwise.

@examples[#:eval my-eval
(define tx '(div [[id "top"][class "red"]] "Hello" (p "World")))
(attrs-have-key? tx 'id)
(attrs-have-key? tx 'grackle)
]

@defproc[
(attr-ref
[tx txexpr?]
[key can-be-txexpr-attr-key?])
txexpr-attr-value?]
Given a @racket[_key], look up the corresponding @racket[_value] in the attributes of a @racket[_txexpr]. Asking for a nonexistent key produces an error.

@examples[#:eval my-eval
(attr-ref tx 'class)
(attr-ref tx 'id)
(attr-ref tx 'nonexistent-key)
]


@defproc[
(attr-set
[tx txexpr?]
[key can-be-txexpr-attr-key?]
[value txexpr-attr-value?])
txexpr?]
Given a @racket[_txexpr], set the value of attribute @racket[_key] to @racket[_value]. The function returns the updated @racket[_txexpr].

@examples[#:eval my-eval
(define tx '(div [[class "red"][id "top"]] "Hello" (p "World")))
(attr-set tx 'id "bottom")
(attr-set tx 'class "blue")
(attr-set (attr-set tx 'id "bottom") 'class "blue")
]


@defproc[
(merge-attrs
[attrs (listof can-be-txexpr-attrs?)] ...)
txexpr-attrs?]
Combine a series of attributes into a single @racket[_txexpr-attrs] item. This function addresses three annoyances that surface in working with txexpr attributes. 

@itemlist[#:style 'ordered
@item{You can pass the attributes in multiple forms. See @racket[can-be-txexpr-attrs?] for further details.}

@item{Attributes with the same name are merged, with the later value taking precedence (i.e., @racket[hash] behavior). }

@item{Attributes are sorted in alphabetical order.}]

@examples[#:eval my-eval
(define tx '(div [[id "top"][class "red"]] "Hello" (p "World")))
(define tx-attrs (get-attrs tx))
tx-attrs
(merge-attrs tx-attrs 'editable "true")
(merge-attrs tx-attrs 'id "override-value")
(define my-attr '(id "another-override"))
(merge-attrs tx-attrs my-attr)
(merge-attrs my-attr tx-attrs)
]

@defproc[
(remove-attrs
[tx txexpr?])
txexpr?]
Recursively remove all attributes.

@examples[#:eval my-eval
(define tx '(div [[id "top"]] "Hello" (p [[id "lower"]] "World")))
(remove-attrs tx)
]

@defproc[
(map-elements
[proc procedure?]
[tx txexpr?])
txexpr?]
Recursively apply @racket[_proc] to all elements, leaving tags and attributes alone. Using plain @racket[map] will only process elements at the top level of the current @racket[_txexpr]. Usually that's not what you want.

@examples[#:eval my-eval
(define tx '(div "Hello!" (p "Welcome to" (strong "Mars"))))
(define upcaser (λ(x) (if (string? x) (string-upcase x) x)))
(map upcaser tx)
(map-elements upcaser tx)
]

In practice, most @racket[_xexpr-element]s are strings. But woe befalls those who pass string procedures to @racket[map-elements], because an @racket[_xexpr-element] can be any kind of @racket[xexpr?], and an @racket[xexpr?] is not necessarily a string.

@examples[#:eval my-eval
(define tx '(p "Welcome to" (strong "Mars" amp "Sons")))
(map-elements string-upcase tx)
(define upcaser (λ(x) (if (string? x) (string-upcase x) x)))
(map-elements upcaser tx)
]

@defproc[
(map-elements/exclude
[proc procedure?]
[tx txexpr?]
[exclude-test (txexpr? . -> . boolean?)])
txexpr?]
Like @racket[map-elements], but skips any @racket[_txexprs] that evaluate to @racket[#t] under @racket[_exclude-test]. The @racket[_exclude-test] gets a whole txexpr as input, so it can test any of its parts.

@examples[#:eval my-eval
(define tx '(div "Hello!" (p "Welcome to" (strong "Mars"))))
(define upcaser (λ(x) (if (string? x) (string-upcase x) x)))
(map-elements upcaser tx)
(map-elements/exclude upcaser tx (λ(x) (equal? (get-tag x) 'strong)))
]

Be careful with the wider consequences of exclusion tests. When @racket[_exclude-test] is true, the @racket[_txexpr] is excluded, but so is everything underneath that @racket[_txexpr]. In other words, there is no way to re-include (un-exclude?) elements nested under an excluded element.

@examples[#:eval my-eval
(define tx '(div "Hello!" (p "Welcome to" (strong "Mars"))))
(define upcaser (λ(x) (if (string? x) (string-upcase x) x)))
(map-elements upcaser tx)
(map-elements/exclude upcaser tx (λ(x) (equal? (get-tag x) 'p)))
(map-elements/exclude upcaser tx (λ(x) (equal? (get-tag x) 'div)))
]

@defproc[
(splitf-txexpr
[tx txexpr?]
[pred procedure?])
(values txexpr? (listof txexpr-element?))]
Recursively descend through @racket[_txexpr] and extract all elements that match @racket[_pred]. Returns two values: a @racket[_txexpr] with the matching elements removed, and the list of matching elements. Sort of esoteric, but I've needed it more than once, so here it is.

@examples[#:eval my-eval
(define tx '(div "Wonderful day" (meta "weather" "good") "for a walk"))
(define remove? (λ(x) (and (txexpr? x) (equal? 'meta (get-tag x)))))
(splitf-txexpr tx remove?)
]


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/txexpr"]{http://github.com/mbutterick/txexpr}. Suggestions & corrections welcome.

