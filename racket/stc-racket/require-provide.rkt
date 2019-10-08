#lang racket/base

;;  --------------------    require+provide.

(require
    (for-syntax
        racket/base
        syntax/stx
    )
)

(provide
    require+provide
)

;;  This may not work with all require forms.

(define-for-syntax (require->provide stx)
    (syntax-case stx (only-in except-in)
        [   (only-in Module Symbol ...)
            #'(combine-out Symbol ...)
        ]
        ;;  No need to except-out the Symbol if it is never imported.
        [   (except-in Module Symbol ...)
            #'(all-from-out Module)
        ]
        [   (For-Phase Require ...)
            #`(For-Phase #,@(stx-map require->provide #'(Require ...)))
        ]
        [   Module
            #'(all-from-out Module)
        ]
    ))

(define-syntax (require+provide stx)
    (syntax-case stx ()
        [   (_ Require ...)
            #`(begin
                (require Require ...)
                (provide #,@(stx-map require->provide #'(Require ...)))
            )
        ]))
