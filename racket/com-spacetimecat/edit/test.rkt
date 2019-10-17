#lang s-exp "lang.rkt"

(require
    rackunit
    racket/gui/base
    "eval.rkt"
)

(provide main)

(define (main)
    (new (class frame%
            (super-new)
            (define/override (on-subwindow-event r e)
                (displayln (eval/string "an-undefined-symbol" (make-base-namespace)))
                (super on-subwindow-event r e)
            )
            (send this show #t)
        )
        [label "Test"] [width 640] [height 480]
    )
)
