#lang racket/base

(require "../racket/require-provide.rkt")

(provide (all-from-out "../racket/require-provide.rkt"))

(require+provide
    (except-in racket/base
        λ
    )
    racket/string
    racket/syntax
)
