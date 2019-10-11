#lang s-exp "lang.rkt"

;;  --------------------    Graphical user interface.

(require
    "core.rkt"
    "gui.rkt"
)

(provide main)

(define (main)
    ;;  <test>
    (println (parse-shortcut 'C-M-q))
    (println (parse-shortcut 'C-M-Q))
    ;;  </test>

    (start-gui)
)
