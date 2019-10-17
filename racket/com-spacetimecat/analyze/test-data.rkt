#lang s-exp "lang.rkt"

(require
    (for-syntax racket/base)
    racket/class
    racket/path
    "outline.rkt"
)

(begin-for-syntax
    (require (for-syntax racket/base))
    (define s 100)
    (define-for-syntax fs 100)
)

(define-for-syntax fs 100)

(define f 1)
(define (g x) 2)
(define h (λ x -> x))
(define i (lambda (x) x))
(define c% (class object% (super-new)))
(define foo (let ((x 100) (y 200)) (+ x y)))

(class object% (super-new))
