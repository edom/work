#lang s-exp "lang.rkt"

(provide
    write-log
)

;;  Pro: Disabled logging has zero runtime overhead,
;;  Con: Toggling logging requires regenerating the code.

(begin-for-syntax
    (define LOG_ENABLED #t)
)

(define-syntax (write-log stx)
    [if LOG_ENABLED
        (syntax-case stx ()
            [(_ Message)
                #'(let [
                        (time (current-date))
                        (level "INFO")
                    ]
                    (printf "~a ~a ~s~n" (format-date/iso time) level Message))
            ]
        )
        #'(void)])
