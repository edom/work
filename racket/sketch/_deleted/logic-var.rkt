#lang racket/base

;;  --------------------    Logic variables.

(require
    (only-in racket/match
        match
    )
)

(provide (all-defined-out))

(struct Var (bound? referent) #:transparent #:mutable)

(define (fresh)
    (Var #f #f)
)

(define (dereference term)
    (match term
        ((Var #f _)
            term
        )
        ((Var _ referent)
            (dereference referent)
        )
        (_ term)
    )
)

(define (unset! var)
    (set-Var-bound?! var #f)
    (set-Var-referent! var #f)
)

(define (unset-all! vars)
    (for-each unset! vars)
)

(define (bind! var referent)
    (set-Var-bound?! var #t)
    (set-Var-referent! var referent)
)

(define (unbound-Var? x)
    (and (Var? x) (not (Var-bound? x)))
)
