#lang s-exp "lang.rkt"

;;  As a library, we do not reexport racket/base at phase level 0.

(require+provide/all
    (for-syntax
        racket/base
    )
    stc-racket/racket-extra
    "object.rkt"
    "define.rkt"
    "for.rkt"
)
