#lang s-exp "lang.rkt"

(require
    racket/file
    "editor.rkt"
    "outline.rkt"
    "option.rkt"
)

(provide
    editor-frame%
)

;;  The frame also functions as the hub in a hub-and-spokes network.
;;  Related terms: hub, bus (event bus), control, orchestrator,
;;  brain, mind, director, organizer.

(define editor-frame% (class frame%

    (super-new [label "Editor"])

    ;;  Static aspects: children, layout, dialogs, and menu bar.

    (define v-panel (new vertical-panel% [parent this]))
        (define h-panel-main (new horizontal-panel% [parent v-panel]))
            (define outline-view (new outline-view% [parent h-panel-main] [control this]))
            (define main-editor-area (new main-editor-area% [parent h-panel-main] [control this]))
        (define h-panel-sta (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (define status-indicator (new message% [parent h-panel-sta] [label ""] [auto-resize #t]))

    (define quick-open-dialog (new path-option-dialog% [label "Quick Open File"]
        [after-submit (λ paths -> (send this open-files paths))]))

    ;;  The menu should not be too big.
    ;;  Features should be disclosed gradually.

    (install-menu-bar this)

    ;;  TODO: Enable some layout customization (especially resizing)
    (define (interpret layout)
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
        (loop this layout))

    (define         default-namespace               (current-namespace))
    (define/public  (get-default-eval-namespace)    default-namespace)
    (define/public  (set-default-eval-namespace ns) (set! default-namespace ns))

    ;;  Commands.

    (define/forward     (complete-word-near-cursor) main-editor-area)
    (define/forward     (evaluate-current-buffer) main-editor-area)
    (define/forward     (focus-on-command-input) main-editor-area)
    (define/forward     (focus-on-main-editor) main-editor-area)
    (define/forward     (open-file path) main-editor-area)
    (define/forward     (open-target path position) main-editor-area)
    ;;  Is this the right way to request the application to quit?
    (define/public      (quit) (send this on-exit))
    (define/forward     (save-file) main-editor-area)
    (define/public      (set-status-message str) (send status-indicator set-label str))
    (define/override    (show bool)
        (when bool (focus-on-main-editor))
        (super show bool))

    (define/public (quick-open-file) (send* quick-open-dialog [clear-query] [show #t]))

    ;;  Reactions.

    (define/public (before-open-file buf path) (send this set-label (path->string path)))
    (define/public (after-open-file buf path) (send main-editor-area after-open-file buf path))
    (define/public (after-outline-change buf old new) (send outline-view set-outline new))

    ;;  TODO: Editor tabs, multiple buffers.

    (define/public (open-files paths)
        (for-each (λ path -> (open-file path)) paths)
        #t)
    (define/public (open-file/ask)
        ;; TODO: dir should be the directory of the current file.
        (define dir #f)
        (define path (get-file #f this dir))
        (when path (open-file path)))
))

(define (install-menu-bar frame)
    (define mb (new menu-bar% [parent frame]))
    (define file (new menu% [parent mb] [label "&File"]))
        (new menu-item% [parent file] [label "&New tab"]
            [callback (λ source event -> (TODO))]
            [shortcut-prefix '(ctl)] [shortcut #\n])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Quick open file"]
            [callback (λ source event -> (send frame quick-open-file))]
            [shortcut-prefix '(ctl)] [shortcut #\p])
        (new menu-item% [parent file] [label "&Open file..."]
            [callback (λ source event -> (send frame open-file/ask))]
            [shortcut-prefix '(ctl)] [shortcut #\o])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Save tab"]
            [callback (λ source event -> (send frame save-file))]
            [shortcut-prefix '(ctl)] [shortcut #\s])
        (new menu-item% [parent file] [label "Save tab &as..."]
            [callback (λ source event -> (TODO))])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Close tab"]
            [callback (λ source event -> (TODO))]
            [shortcut-prefix '(ctl)] [shortcut #\w])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "&Evaluate tab"]
            [callback (λ source event -> (send frame evaluate-current-buffer))]
            [shortcut-prefix '(ctl)] [shortcut #\e])
        (new separator-menu-item% [parent file])
        (new menu-item% [parent file] [label "Q&uit entire application"]
            [callback (λ source event -> (send frame quit))]
            [shortcut-prefix '(ctl)] [shortcut #\q])
    (define edit (new menu% [parent mb] [label "&Edit"]))
        (define edit-complete (new menu-item% [parent edit] [label "Complete word near cursor"]
                [callback (λ source event -> (send frame complete-word-near-cursor))]
                [shortcut-prefix '(ctl)] [shortcut #\space]))
        (new separator-menu-item% [parent edit])
        (define edit-enter (new menu-item% [parent edit] [label "Enter command"]
                [callback (λ source event -> (send frame focus-on-command-input))]
                [shortcut-prefix '(meta)] [shortcut #\x]))
        (new separator-menu-item% [parent edit])
        (append-editor-operation-menu-items edit #t)
    (define navigate (new menu% [parent mb] [label "&Navigate"]))
        (define navigate-go-to-definition (new menu-item% [parent navigate] [label "TODO: Define word under cursor"]
            [callback (λ source event -> (void))]))
        (define navigate-go-to-line (new menu-item% [parent navigate] [label "TODO: Go to line"]
            [callback (λ source event -> (void))]))
    (void))
