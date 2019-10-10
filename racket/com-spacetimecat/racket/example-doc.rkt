#lang racket/base

(require "doc.rkt")

(provide main)

(define (f x)
    ($doc "add one")
    (+ x 1)
)

($doc #:for g "add two")
(define (g x) (+ x 2))

;;  Test literal matching.
(require (rename-in "doc.rkt" [$doc DOC]))
(DOC #:for h "add three")
(define (h x) (+ x 3))

(define (main)
    ;;  How do we make this path relative to this module
    ;;  instead of relative to the working directory?
    (define docs (read-docs-from-file "com-spacetimecat/racket/example-doc.rkt"))
    (for ([doc (in-list docs)])
        (println doc)
    )
)
