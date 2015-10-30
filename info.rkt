#lang info
(define collection "txexpr")
(define deps '("base" "sugar" "rackunit-lib"))
(define update-implies '("sugar"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/txexpr.scrbl" ())))
(define compile-omit-paths '("tests.rkt"))