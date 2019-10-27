#lang s-exp "lang.rkt"

(require
    "editor.rkt"
    "outline.rkt"
)

(provide
    editor-frame%
)

(define editor-frame% (class frame%
    (init-field session)
    (init      [label "Editor"])
    (super-new [label label])

    ;;  Static aspects: children, layout, dialogs, and menu bar.

    (define v-panel (new vertical-panel% [parent this]))
        (define h-panel-main (new horizontal-panel% [parent v-panel]))
            (field [outline-view (new outline-view% [parent h-panel-main] [control session])])
            (field [main-editor-area (new main-editor-area% [parent h-panel-main] [control session])])
        (define h-panel-sta (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (field [status-indicator (new message% [parent h-panel-sta] [label ""] [auto-resize #t])])

    (install-menu-bar session this)

    (define/override (show bool)
        (when bool (send session focus-on-main-editor))
        (super show bool))
))

;;  The menu should not be too big.
;;  Features should be disclosed gradually.
(define (install-menu-bar session frame)
    (define mb (new menu-bar% [parent frame]))
    (define file (new menu% [parent mb] [label "&File"]))
        (new menu-item% [parent file] [label "&New tab"]
            [callback (λ source event -> (TODO))]
            [shortcut-prefix '(ctl)] [shortcut #\n])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Quick open file"]
            [callback (λ source event -> (send session quick-open-file))]
            [shortcut-prefix '(ctl)] [shortcut #\p])
        (new menu-item% [parent file] [label "&Open file..."]
            [callback (λ source event -> (send session open-file/ask))]
            [shortcut-prefix '(ctl)] [shortcut #\o])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Save tab"]
            [callback (λ source event -> (send session save-file))]
            [shortcut-prefix '(ctl)] [shortcut #\s])
        (new menu-item% [parent file] [label "Save tab &as..."]
            [callback (λ source event -> (TODO))])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Close tab"]
            [callback (λ source event -> (TODO))]
            [shortcut-prefix '(ctl)] [shortcut #\w])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Evaluate tab"]
            [callback (λ source event -> (send session evaluate-current-buffer))]
            [shortcut-prefix '(ctl)] [shortcut #\e])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "Q&uit entire application"]
            [callback (λ source event -> (send session quit))]
            [shortcut-prefix '(ctl)] [shortcut #\q])
    (define edit (new menu% [parent mb] [label "&Edit"]))
        (define edit-complete (new menu-item% [parent edit] [label "Complete word near cursor"]
                [callback (λ source event -> (send session complete-word-near-cursor))]
                [shortcut-prefix '(ctl)] [shortcut #\space]))
        (new separator-menu-item% [parent edit])
        (define edit-enter (new menu-item% [parent edit] [label "Enter command"]
                [callback (λ source event -> (send session focus-on-command-input))]
                [shortcut-prefix '(meta)] [shortcut #\x]))
        (new separator-menu-item% [parent edit])
        (append-editor-operation-menu-items edit #t)
    (define navigate (new menu% [parent mb] [label "&Navigate"]))
        (define navigate-go-to-definition (new menu-item% [parent navigate] [label "TODO: Define word under cursor"]
            [callback (λ source event -> (void))]))
        (define navigate-go-to-line (new menu-item% [parent navigate] [label "TODO: Go to line"]
            [callback (λ source event -> (void))]))
    (void))


;;  TODO: Enable some layout customization (especially resizing)
(define (interpret root layout)
    (define (loop parent layout)
        (match layout
            [`(hflow ,children)
                (loop (new horizontal-panel% [parent parent] [stretchable-height #f]) children)
            ]
            [`(vflow ,children)
                (loop (new vertical-panel% [parent parent] [stretchable-width #f]) children)
            ]
            [`(outline-view)
                (TODO)
            ]
            [`(main-editor)
                (TODO)
            ]
        ))
    (loop root layout))
