#lang s-exp "lang.rkt"

(require
    "frame.rkt"
)

(provide
    start-gui
)

(define (start-gui)
    (define frame (new my-editor-frame%))

    (define path "com-spacetimecat/edit/gui/gui.rkt")
    (send frame load-file path)
    (send frame focus-on-main-editor)
    (send frame show #t)
)
