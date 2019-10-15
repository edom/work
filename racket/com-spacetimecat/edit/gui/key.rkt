#lang s-exp "lang.rkt"

(provide
    key-event->combination
    format-key-event
)

(define (key-event->combination e)
    (define key (send e get-key-code))
    (define alt (send e get-alt-down))
    (define ctrl (send e get-control-down))
    (append
        (if ctrl '(ctrl) '())
        (if alt '(alt) '())
        (list key)
    )
)

(define (format-key-event e)
    (define key (send e get-key-code))
    (~a
        (if (send e get-control-down) "C-" "")
        (if (send e get-alt-down) "M-" "")
        (if (send e get-shift-down) "S-" "")
        key
        (if (send e get-caps-down) " (caps lock on)" "")
    )
)
