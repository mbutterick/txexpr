#lang racket/base
(require sugar/include sugar/define xml)

(define+provide+safe (txexpr? x)
  (any/c . -> . boolean?)
  (with-handlers ([exn:fail? (λ(exn) #f)])
    (and (validate-txexpr x) #t)))

(define+provide+safe (txexpr-attr? x)
  (any/c . -> . boolean?)
  (match x
    [(list (? symbol?) (? string?)) #t]
    [else #f]))

(include-without-lang-line "../typed/txexpr/main.rkt")
