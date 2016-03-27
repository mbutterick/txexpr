#lang info
(define version "0.2")
(define collection "txexpr")
(define deps '("base" ["sugar" #:version "0.2"] "rackunit-lib"))
(define update-implies '("sugar"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-doc"))
(define scribblings '(("scribblings/txexpr.scrbl" ())))
(define compile-omit-paths '("tests.rkt"))