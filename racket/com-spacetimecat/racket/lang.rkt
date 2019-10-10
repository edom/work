#lang racket/base

(require
    "require-provide.rkt"
)

(provide
    (except-out (all-from-out racket/base)
        λ
    )
    (all-from-out
        "require-provide.rkt"
    )
)

(require+provide
    (for-syntax
        racket/base
        syntax/parse
    )
    racket/format
    syntax/parse/define
    "doc.rkt"
)
