#lang s-exp "lang.rkt"

(require
    (for-syntax
        racket/syntax
    )
)

(require+provide
    racket/class
)

(provide
    define-property
)

(define-syntax-parser define-property
    [   (_ $:id init:expr)
        (with-syntax (
                [get-$ (format-id #'$ "get-~a" #'$)]
                [set-$ (format-id #'$ "set-~a" #'$)]
                [before-$-change (format-id #'$ "before-~a-change" #'$)]
                [after-$-change (format-id #'$ "after-~a-change" #'$)]
            )
            #'(begin
                (define $ init)
                (define/public (get-$) $)
                (define/public (set-$ new)
                    (define old $)
                    (before-$-change old new)
                    (set! $ new)
                    (after-$-change old new)
                )
                (define/public (before-$-change old new) (void))
                (define/public (after-$-change old new) (void))
            )
        )
    ]
)
