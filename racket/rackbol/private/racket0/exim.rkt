#lang racket

(provide
    require+provide/only
    require+provide/all
)

;;  These forms do not accept "for-syntax" require form.
;;  If you need that, wrap this in a begin-for-syntax instead.
;;  Or, use Racket's "require" and "provide".

(define-syntax-rule (require+provide/only [Module Symbol ...] ...)
    [begin
        (begin
            (require (only-in Module Symbol ...))
            (provide Symbol ...)
        ) ...
    ])

(define-syntax-rule (require+provide/all Module ...)
    [begin
        (begin
            (require Module)
            (provide (all-from-out Module))
        ) ...])
