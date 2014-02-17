#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt" xml))

@(define my-eval (make-base-eval))
@(my-eval `(require tagged-xexpr xml))


@title{tagged-xexpr}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

A set of small but handy functions for improving the readability and reliability of programs that operate on tagged X-expressions (aka tagged-xexprs).

@section{Installation}

At the command line:
@verbatim{raco pkg install tagged-xexpr}

After that, you can update the package from the command line:
@verbatim{raco pkg update tagged-xexpr}

@section{What’s a tagged-xexpr?}

It's an X-expression with the following grammar:

@racketgrammar*[
#:literals (cons list symbol? string? xexpr?)
[tagged-xexpr (list tag (list attr ...) element ...)
      (cons tag (list element ...))]
[tag symbol?]
[attr (list symbol? string?)]
[element xexpr?]
]


A tagged X-expression is a list with a symbol in the first position — the @italic{tag} — followed by a series of @italic{elements}, which are other X-expressions. Optionally, a tagged X-expression can have a list of @italic{attributes} in the second position.

@examples[#:eval my-eval
(tagged-xexpr? '(span "Brennan" "Dale"))
(tagged-xexpr? '(span "Brennan" (em "Richard") "Dale"))
(tagged-xexpr? '(span [[class "hidden"][id "names"]] "Brennan" "Dale"))
(tagged-xexpr? '(span lt gt amp))
(tagged-xexpr? '("We really" "should have" "a tag"))
(tagged-xexpr? '(span [[class not-quoted]] "Brennan")) 
(tagged-xexpr? '(span [class "hidden"] "Brennan" "Dale")) 
   ]
   
The last one is a common mistake. Because the key–value pair is not enclosed in a @racket[list], it's interpreted as a nested tagged-xexpr within the first tagged-xexpr, as you may not find out until you try to read its attributes:

@margin-note{There's no way of eliminating this ambiguity, short of always requiring an attribute list — empty if necessary — in your tagged-xexpr. See also @racket[xexpr-drop-empty-attributes].}

@examples[#:eval my-eval
(tagged-xexpr-attrs '(span [class "hidden"] "Brennan" "Dale")) 
(tagged-xexpr-elements '(span [class "hidden"] "Brennan" "Dale")) 
]

Tagged X-expressions are most commonly found in HTML & XML documents. Though the notation is different in Racket, the data structure is identical:

@examples[#:eval my-eval
(xexpr->string '(span [[id "names"]] "Brennan" (em "Richard") "Dale"))
(string->xexpr "<span id=\"names\">Brennan<em>Richard</em>Dale</span>")
   ]

After converting to and from HTML, we get back the original X-expression. Well, almost. The brackets turned into parentheses — no big deal, since they mean the same thing in Racket. Also, per its usual practice, @racket[string->xexpr] added an empty attribute list after @racket[em]. This is also benign.

@section{Why not just use @exec{match}, @exec{quasiquote}, and so on?}

If you prefer those, please do. But I've found two benefits to using module functions:

@bold{Readability.} In code that already has a lot of matching and quasiquoting going on, these functions make it easy to see where & how tagged-xexprs are being used.

@bold{Reliability.} Because tagged-xexprs come in two close but not quite equal forms, careful coders will always have to take both cases into account.

The programming is trivial, but the annoyance is real.

@section{Interface}

@defmodule[tagged-xexpr]

@deftogether[(
@defproc[
(tagged-xexpr?
[v any/c])
boolean?]

@defproc[
(tagged-xexpr-tag?
[v any/c])
boolean?]

@defproc[
(tagged-xexpr-attr?
[v any/c])
boolean?]

@defproc[
(tagged-xexpr-element?
[v any/c])
boolean?]

)]
Predicates for @racket[_tagged-xexpr]s that implement this grammar:

@racketgrammar*[
#:literals (cons list symbol? string? xexpr?)
[tagged-xexpr (list tag (list attr ...) element ...)
      (cons tag (list element ...))]
[tag symbol?]
[attr (list symbol? string?)]
[element xexpr?]
]

@deftogether[(

@defproc[
(tagged-xexpr-attrs?
[v any/c])
boolean?]

@defproc[
(tagged-xexpr-elements?
[v any/c])
boolean?]
)]
Shorthand for @code{(listof tagged-xexpr-attr?)} and @code{(listof tagged-xexpr-element?)}.


@defproc[
(tagged-xexpr->values
[tx tagged-xexpr?]) 
(values tagged-xexpr-tag? tagged-xexpr-attrs? tagged-xexpr-elements?)]
Dissolves a @racket[_tagged-xexpr] into its components and returns all three.

@examples[#:eval my-eval
(tagged-xexpr->values '(div))
(tagged-xexpr->values '(div "Hello" (p "World")))
(tagged-xexpr->values '(div [[id "top"]] "Hello" (p "World")))

]

@defproc[
(tagged-xexpr->list
[tx tagged-xexpr?]) 
(list tag attrs elements)]
Like @racket[tagged-xexpr->values], but returns the three components in a list.

@examples[#:eval my-eval
(tagged-xexpr->list '(div))
(tagged-xexpr->list '(div "Hello" (p "World")))
(tagged-xexpr->list '(div [[id "top"]] "Hello" (p "World")))
]

@deftogether[(
@defproc[
(tagged-xexpr-tag
[tx tagged-xexpr?])
tagged-xexpr-tag?]

@defproc[
(tagged-xexpr-attrs
[tx tagged-xexpr?])
tagged-xexpr-attr?]

@defproc[
(tagged-xexpr-elements
[tx tagged-xexpr?])
(listof tagged-xexpr-element?)]
)]
Accessor functions for the individual pieces of a @racket[_tagged-xexpr].

@examples[#:eval my-eval
(tagged-xexpr-tag '(div [[id "top"]] "Hello" (p "World")))
(tagged-xexpr-attrs '(div [[id "top"]] "Hello" (p "World")))
(tagged-xexpr-elements '(div [[id "top"]] "Hello" (p "World")))
]

@defproc[
(make-tagged-xexpr
[tag tagged-xexpr-tag?] 
[attrs tagged-xexpr-attrs? @(empty)]
[elements tagged-xexpr-elements? @(empty)])
tagged-xexpr?]
Assemble a @racket[_tagged-xexpr] from its parts. If you don't have attributes, but you do have elements, you'll need to pass @racket[empty] as the second argument. Note that unlike @racket[xml->xexpr], if the attribute list is empty, it's not included in the resulting expression.

@examples[#:eval my-eval
(make-tagged-xexpr 'div)
(make-tagged-xexpr 'div '() '("Hello" (p "World")))
(make-tagged-xexpr 'div '[[id "top"]])
(make-tagged-xexpr 'div '[[id "top"]] '("Hello" (p "World")))
(define tx '(div [[id "top"]] "Hello" (p "World")))
(make-tagged-xexpr (tagged-xexpr-tag tx) 
(tagged-xexpr-attrs tx) (tagged-xexpr-elements tx))
]

@defproc[
(merge-attrs
[attrs (listof (or/c tagged-xexpr-attr? tagged-xexpr-attrs? 
symbol? string?))] ...)
tagged-xexpr-attrs?]
Combine a series of attributes into a single @racket[_tagged-xexpr-attrs] item. This function addresses three annoyances that surface in working with tagged-xexpr attributes. 

@itemlist[#:style 'ordered
@item{You can pass the attributes in multiple forms. The list of arguments can include single @racket[_xexpr-attr]s, lists of @racket[_xexpr-attr]s (i.e., what you get from @racket[tagged-xexpr-attrs]), or interleaved symbols and strings (each pair will be concatenated into a single @racket[_xexpr-attr]).}

@item{Attributes with the same name are merged, with the later value taking precedence (i.e., @racket[hash] behavior). }

@item{Attributes are sorted in alphabetical order.}]

@examples[#:eval my-eval
(define tx '(div [[id "top"][class "red"]] "Hello" (p "World")))
(define tx-attrs (tagged-xexpr-attrs tx))
tx-attrs
(merge-attrs tx-attrs 'editable "true")
(merge-attrs tx-attrs 'id "override-value")
(define my-attr '(id "another-override"))
(merge-attrs tx-attrs my-attr)
(merge-attrs my-attr tx-attrs)
]

@defproc[
(remove-attrs
[tx tagged-xexpr?])
tagged-xexpr?]
Recursively remove all attributes.

@examples[#:eval my-eval
(define tx '(div [[id "top"]] "Hello" (p [[id "lower"]] "World")))
(remove-attrs tx)
]

@defproc[
(map-elements
[proc procedure?]
[tx tagged-xexpr?])
tagged-xexpr?]
Recursively apply @racket[_proc] to all elements, leaving tags and attributes alone. Using plain @racket[map] will only process elements at the top level of the current @racket[_tagged-xexpr]. Usually that's not what you want.

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



@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/tagged-xexpr"]{http://github.com/mbutterick/tagged-xexpr}. Suggestions & corrections welcome.

