#lang racket/base

(require (except-in scribble/manual/lang
    section
    subsection
))
(require
    "private/scribble-more.rkt"
    (for-label
        racket
        stc-racket
    )
)
(provide
    (all-from-out
        scribble/manual/lang
        "private/scribble-more.rkt"
    )
    (for-label
        (all-from-out
            racket
            stc-racket
        )
    )
)

(module reader scribble/base/reader
    stc-racket/scribble-manual
    #:wrapper1 (lambda (t) (cons 'doc (t)))
)
