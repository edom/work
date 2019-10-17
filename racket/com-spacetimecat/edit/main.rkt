#lang s-exp "lang.rkt"

;;  --------------------    Graphical user interface.

(require
    "core.rkt"
    "gui/frame.rkt"
)

(provide main)

(define (main) (start-gui))

;;  For eval.
(define frame #f)

(define-namespace-anchor -anchor)

(define (start-gui)
    (set! frame (new editor-frame% [width 1024] [height 768]))
    (send frame set-eval-namespace (namespace-anchor->namespace -anchor))
    (send frame show #t)
)
