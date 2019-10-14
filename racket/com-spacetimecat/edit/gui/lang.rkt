#lang s-exp "../lang.rkt"

(provide (all-from-out "../lang.rkt"))

;;  Problem: 2019-10-13:
;;  I tried "framework", but it slows down startup.

(require+provide
    racket/gui/base
    "../core.rkt"
)

(provide
    call-with-container-sequence
    with-container-sequence
    clear-container
)

(define (call-with-container-sequence container proc)
    (dynamic-wind
        (λ => send container begin-container-sequence)
        proc
        (λ => send container end-container-sequence)
    )
)

(define-syntax-rule (with-container-sequence container body ...)
    (call-with-container-sequence container (lambda () body ...))
)

(define (clear-container container)
    (with-container-sequence container
        (send container change-children (λ _ -> '()))
    )
)
