#lang s-exp "lang.rkt"

;;  --------------------    Graphical user interface.

(require
    "core.rkt"
    "gui/frame.rkt"
)

(provide main)

(define (main) (start-gui))

(define (start-gui)
    (define frame (new behaving-editor-frame%))
    (send frame show #t)
)
