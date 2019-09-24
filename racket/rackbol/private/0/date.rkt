#lang s-exp "_in.rkt"

(require racket/date)

(provide
    format-date/iso
    ;;  --------------------    racket/date
    current-date
)

(define (format-date/iso x)
    (define offset (date-time-zone-offset x))
    (define-values (offset-hour offset-m) (quotient/remainder offset 3600))
    (define offset-minute (quotient offset-m 60))
    (format "~a-~a-~aT~a:~a:~a~a~a:~a"
        ;;  TODO: left-pad with zero
        (date-year x)
        (date-month x)
        (date-day x)
        (date-hour x)
        (date-minute x)
        (date-second x)
        (if (nonnegative-integer? offset) "+" "")
        offset-hour
        offset-minute
    )
)
