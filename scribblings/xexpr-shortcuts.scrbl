#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt"))

@(define my-eval (make-base-eval))
@(my-eval `(require xexpr-shortcuts))


@title{xexpr-shortcuts}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

Convenience functions for working with X-expressions in Racket.

@section{Installation & updates}

At the command line:
@verbatim{raco pkg install xexpr-shortcuts}

After that, you can update the package from the command line:
@verbatim{raco pkg update xexpr-shortcuts}


@section{Interface}

@defmodule[xexpr-shortcuts]

Hello xexpr-shortcuts.


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/xexpr-shortcuts"]{http://github.com/mbutterick/xexpr-shortcuts}. Suggestions & corrections welcome.

