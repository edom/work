#lang racket/base

(provide
    empty
    bind
    lookup
)

(define (empty) '())

(define (bind ctx name referent)
    (cons (cons name referent) ctx))

(define (lookup ctx name)
    (define pair (assoc name ctx))
    (if pair (cdr pair) (error 'lookup "unbound variable: ~v" name)))
