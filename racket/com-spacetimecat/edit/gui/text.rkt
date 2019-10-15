#lang s-exp "lang.rkt"

(provide
    my-editor-text%
)

(define preferred-faces (list
    "Noto Mono"
    "Droid Sans Mono"
    "Liberation Mono"
    "DejaVu Sans Mono"
    "Courier New"
    "Monospace"
))

(define my-editor-text% (class text%
    (super-new)
    (define/override (default-style-name) "Code")
    (define/override (on-event e)
        (define admin (send this get-admin))
        (when (and admin (send e button-down? 'right))
            (define menu (new popup-menu%))
            (new menu-item% [parent menu] [label "TODO: Define <word>"] [callback (λ r e => void)])
            (new menu-item% [parent menu] [label "TODO: Open <module>"] [callback (λ r e => void)])
            (new menu-item% [parent menu] [label "TODO: Show documentation for <word>"] [callback (λ r e => void)])
            (new menu-item% [parent menu] [label "TODO: Find what uses <word/module>"] [callback (λ r e => void)])
            (new menu-item% [parent menu] [label "TODO: Find what is used by <word/module>"] [callback (λ r e => void)])
            (new menu-item% [parent menu] [label "TODO: Show <word/module> dependencies/uses/used-by"] [callback (λ r e => void)])
            (send admin popup-menu menu (send e get-x) (send e get-y))
        ))
    (define (initialize-styles)
        (define available-faces (get-face-list 'mono))
        (define face (choose-first
            #:from preferred-faces
            #:in available-faces
            #:or #f
        ))
        ;;  TODO: Set font size.
        (define style-list (send this get-style-list))
        (define style-0 (send style-list basic-style))
        (define delta-0 (new style-delta%))
        (send delta-0 set-face face)
        (send delta-0 set-family 'modern) ;; Racketspeak for monospace.
        (define style-1 (send style-list find-or-create-style style-0 delta-0))
        (send style-list new-named-style "Code" style-1)
        (send this set-styles-sticky #f)
    )
    (initialize-styles)
))
