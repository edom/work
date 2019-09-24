#lang racket/base

(require [except-in racket
    log
    λ
])
(provide [all-from-out racket])

;;  --------------------    require/provide.
;;  These forms do not accept "for-syntax" require form.
;;  If you need that, wrap this in a begin-for-syntax instead.
;;  Or, use Racket's "require" and "provide".

(provide
    require+provide/only
    require+provide/all
)

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

;;  --------------------    Other.

(require+provide/all syntax/parse)
(require+provide/all syntax/parse/define)
(require+provide/only
    [stc-racket/racket-extra
        λ LET LET* LETREC LETREC* GROUP deconstruct with-environment ->string
    ]
    [racket/include
        include
    ]
    [racket/pretty
        pretty-print
    ]
)
