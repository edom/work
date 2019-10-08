#lang racket/base

(require stc-racket/require-provide)

(provide (all-from-out stc-racket/require-provide))

(require+provide
    (except-in racket/base
        λ
    )
    racket/string
    racket/syntax
    "syntax-parse.rkt"
)
