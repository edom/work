#lang racket/base

;;  This is racket/base with some enhancements
;;  that are often used in practical applications
;;  and that do not take too long to load.

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
