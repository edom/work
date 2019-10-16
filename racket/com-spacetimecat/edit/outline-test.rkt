#lang s-exp "lang.rkt"

(require
    (for-syntax racket/base)
    racket/class
    racket/path
    racket/pretty
    "outline.rkt"
)

(begin-for-syntax
    (require (for-syntax racket/base))
    (define s 100)
    (define-for-syntax fs 100)
)

(define-for-syntax fs 100)

(provide main)

(define-syntax ($__FILE__/path stx)
    (datum->syntax stx (syntax-source stx)))

(define-syntax ($__FILE__/string stx)
    (datum->syntax stx (path->string (syntax-source stx))))

(define f 1)
(define (g x) 2)
(define h (λ x -> x))
(define i (lambda (x) x))
(define c% (class object% (super-new)))
(define foo (let ((x 100) (y 200)) (+ x y)))

(class object% (super-new))

(define (main)
    (define outline (compute-file-outline
        ;;  XXX: ".." is an abuse.
        ;(simplify-path (build-path $__FILE__/path ".." "frame.rkt") #f)
        (simplify-path $__FILE__/path #f)
    ))
    ;(pretty-print (syntax->datum outline))
    (pretty-print outline)
)
