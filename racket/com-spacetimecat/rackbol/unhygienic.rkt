#lang racket

(provide (all-defined-out))

(require (for-syntax syntax/strip-context))

(define-syntax (UNHYGIENIC stx)
    (syntax-case stx ()
        [(_ Arg ...)
            (replace-context stx #'(begin Arg ...))
        ]
    )
)
