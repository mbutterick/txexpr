#lang info
(define version "0.2")
(define collection 'multi)
(define deps '("base" ["sugar" #:version "0.2"] "rackunit-lib"))
(define update-implies '("sugar"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-doc"))
(define scribblings '(("txexpr/scribblings/txexpr.scrbl" ())))
(define compile-omit-paths '("txexpr/tests.rkt"))