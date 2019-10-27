#lang s-exp "lang.rkt"

;;  --------------------    Graphical user interface.

(require
    "gui/session-imp.rkt"
)

(provide main)

(define (main) (start-gui))

;;  For eval.
(define session #f)

(define-namespace-anchor -anchor)

(define (start-gui)
    (set! session (new session%
        [default-eval-namespace (namespace-anchor->namespace -anchor)])))
