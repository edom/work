#lang s-exp "typed.rkt"

2
3
(:type-of 1)
(:type-of "foo")
(define x 10)
(define y x)
(define z "100")
(:type-of x)

