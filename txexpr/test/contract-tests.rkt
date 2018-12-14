#lang racket/base

(require (submod "../main.rkt" safe))
(module+ test
  (require rackunit))

(module+ test
  (check-exn (regexp (string-append
                      "txexpr.*: contract violation\n"
                      " *expected: txexpr-tag\\?\n"
                      " *given: 4\n"
                      " *in: the 1st argument.*"
                      " *blaming: .*test/contract-tests.rkt"))
             (λ () (txexpr 4)))
  )

