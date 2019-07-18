; to load this, require this file from load.rkt

(module error2 "error1.rkt"

    (define-syntax-rule (printexps1 exp ...)
        (begin
            (printf "~s~n" (quote exp)) ...
        )
    )

    ; This produces wrong result if error1.rkt does not provide (for-syntax ...).
    (printexps1 (+ 1 2) (+ 3 4))
)
