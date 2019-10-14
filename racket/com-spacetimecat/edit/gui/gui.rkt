#lang s-exp "lang.rkt"

(require
    "frame.rkt"
)

(provide
    start-gui
)

(define (start-gui)
    (define frame (new my-editor-frame%))

    (send frame focus-on-main-editor)
    (send frame show #t)
)
