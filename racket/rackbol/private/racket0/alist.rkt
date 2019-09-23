#lang racket

(provide
    alist-ref
)

(define (alist-ref alist key)
    (define pair (assoc key alist))
    (if pair    (cdr pair)
                (error 'alist-ref "undefined key: ~a" key) ))
