#lang s-exp "lang.rkt"

(require+provide
    racket/date
)

(provide
    format-date/iso
)

(define (format-date/iso x)
    (define offset (date-time-zone-offset x))
    (define-values (offset-hour offset-m) (quotient/remainder (abs offset) 3600))
    (define offset-minute (quotient offset-m 60))
    (format "~a-~a-~aT~a:~a:~a~a~a:~a"
        (~r (date-year x) #:min-width 4 #:pad-string "0")
        (~r (date-month x) #:min-width 2 #:pad-string "0")
        (~r (date-day x) #:min-width 2 #:pad-string "0")
        (~r (date-hour x) #:min-width 2 #:pad-string "0")
        (~r (date-minute x) #:min-width 2 #:pad-string "0")
        (~r (date-second x) #:min-width 2 #:pad-string "0")
        (if (>= offset 0) '+ '-)
        (~r offset-hour #:min-width 2 #:pad-string "0")
        (~r offset-minute #:min-width 2 #:pad-string "0")
    ))
