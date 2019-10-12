#lang s-exp "lang.rkt"

;;  --------------------    Graphical user interface.

(require
    "core.rkt"
    "gui/gui.rkt"
)

(provide main)

(define (main)
    (start-gui)
)
