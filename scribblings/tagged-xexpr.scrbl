#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt"))

@(define my-eval (make-base-eval))
@(my-eval `(require tagged-xexpr))


@title{tagged-xexpr}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

Convenience functions for working with X-expressions in Racket.

@section{Installation & updates}

At the command line:
@verbatim{raco pkg install tagged-xexpr}

After that, you can update the package from the command line:
@verbatim{raco pkg update tagged-xexpr}


@section{Interface}

@defmodule[tagged-xexpr]

Hello tagged-xexpr.


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/tagged-xexpr"]{http://github.com/mbutterick/tagged-xexpr}. Suggestions & corrections welcome.

