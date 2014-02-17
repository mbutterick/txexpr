#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt" xml))

@(define my-eval (make-base-eval))
@(my-eval `(require tagged-xexpr xml))


@title{tagged-xexpr}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

Convenience functions for working with tagged X-expressions.

@section{Installation}

At the command line:
@verbatim{raco pkg install tagged-xexpr}

After that, you can update the package from the command line:
@verbatim{raco pkg update tagged-xexpr}

@section{What’s a tagged X-expression?}

It's an X-expression with the following grammar:

@racketgrammar[
#:literals (cons list valid-char?)
tagged-xexpr (list symbol (list (list symbol string) ...) xexpr ...)
      (cons symbol (list xexpr ...))
]

A tagged X-expression has a symbol in the first position — the @italic{tag} — followed by a series of other X-expressions. Optionally, a tagged X-expression can have a list of @italic{attributes} in the second position, which are pairs of symbols and strings.

@examples[#:eval my-eval
(tagged-xexpr? '(tag "Brennan" "Dale"))
(tagged-xexpr? '(tag "Brennan" (tag2 "Richard") "Dale"))
(tagged-xexpr? '(tag [[key "value"][key2 "value"]] "Brennan" "Dale"))
(tagged-xexpr? '(tag symbols are fine))
(tagged-xexpr? '("No" "tag" "in front"))
(tagged-xexpr? '(tag [[bad attr-value]] "string")) 
(tagged-xexpr? '(tag [key "value"] "Brennan")) 
   ]
   
Be careful with the last one. Because the key–value pair is not enclosed in a @racket[list], it's interpreted as a nested @racket[_tagged-xexpr] within the first, as you may not find out until you try to read its attributes:

@margin-note{There's no way of eliminating this ambiguity, short of always requiring an attribute list — even empty — in your tagged X-expression. See also @racket[xexpr-drop-empty-attributes].}

@examples[#:eval my-eval
(tagged-xexpr-attr '(tag [key "value"] "Brennan")) 
]

Tagged X-expressions are most commonly seen in XML & HTML documents. Though the notation is different in Racket, the data structure is identical:

@examples[#:eval my-eval
(xexpr->string '(p [[foo "bar"]] "Brennan" (em "Richard") "Dale"))
(string->xexpr "<p foo=\"bar\">Brennan<em>Richard</em>Dale</p>")
   ]

After converting to and from HTML, you get back your original X-expression. Well, not quite. The brackets turned into parentheses — no big deal, since they mean the same thing in Racket. Also true that @racket[string->xexpr] added an empty attribute list after @racket[em]. This is standard procedure, and also benign.

@section{Interface}

@defmodule[tagged-xexpr]

@defproc[
(tagged-xexpr?
[v any/c])
boolean?]
Simple predicate for functions that operate on @racket[tagged-xexpr]s.


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/tagged-xexpr"]{http://github.com/mbutterick/tagged-xexpr}. Suggestions & corrections welcome.

