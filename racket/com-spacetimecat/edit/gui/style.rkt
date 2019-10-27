#lang s-exp "lang.rkt"

(provide
    initialize-styles
)

(define preferred-faces (list
    "Noto Mono"
    "Droid Sans Mono"
    "Liberation Mono"
    "DejaVu Sans Mono"
    "Courier New"
    "Monospace"
))

(define (initialize-styles text)
    (define available-faces (get-face-list 'mono))
    (define face (choose-first
        #:from preferred-faces
        #:in available-faces
        #:or #f
    ))
    ;;  TODO: Set font size.
    (define style-list (send text get-style-list))
    (define style-0 (send style-list basic-style))
    (define delta-0 (new style-delta%))
    (send delta-0 set-face face)
    (send delta-0 set-family 'modern) ;; Racketspeak for monospace.
    (define style-1 (send style-list find-or-create-style style-0 delta-0))
    ;;  2019-10-23: Fragility: The name must not already exist in the style list.
    (send style-list new-named-style "Code" style-1)
    (send text set-styles-sticky #f))
