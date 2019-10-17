#lang s-exp "lang.rkt"

(require
    racket/file
    racket/match
    "editor.rkt"
    "outline.rkt"
    "quick-open.rkt"
)

(provide
    editor-frame%
)

;;  The frame also functions as the hub in a hub-and-spokes network.
;;  Related terms: hub, bus (event bus), control, orchestrator,
;;  brain, mind, director, organizer.

(define editor-frame% (class frame%

    (super-new [label "Editor"])

    (define frame this)

    ;;  Static aspects: children, layout, dialogs, and menu bar.

    (define v-panel (new vertical-panel% [parent frame]))
        (define h-panel-main (new horizontal-panel% [parent v-panel]))
            (define outline-view (new outline-view% [parent h-panel-main] [control this]))
            (define main-editor-area (new main-editor-area% [parent h-panel-main] [control frame]))
        (define h-panel-sta (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (define status-indicator (new message% [parent h-panel-sta] [label ""] [auto-resize #t]))

    (define quick-open-dialog (new quick-open-dialog% [control this]))

    ;;  The menu should not be too big.
    ;;  Features should be disclosed gradually.
    ;;  TODO: Generate this from (struct Action (label shortcut callback)).
    (define (install-menu-bar)
        (define mb (new menu-bar% [parent frame]))
        (define file (new menu% [parent mb] [label "&File"]))
            (define file-open (new menu-item% [parent file] [label "&Open"]
                [callback (λ source event => open-file/ask)]
                [shortcut-prefix '(ctl)] [shortcut #\o]))
            (define file-save (new menu-item% [parent file] [label "&Save"]
                [callback (λ source event => save-file)]
                [shortcut-prefix '(ctl)] [shortcut #\s]))
            (define file-quit (new menu-item% [parent file] [label "&Quit"]
                [callback (λ source event => quit)]
                [shortcut-prefix '(ctl)] [shortcut #\Q]))
        (define edit (new menu% [parent mb] [label "&Edit"]))
            (define edit-complete (new menu-item% [parent edit] [label "Complete word near cursor"]
                    [callback (λ source event => complete-word-near-cursor)]
                    [shortcut-prefix '(ctl)] [shortcut #\space]))
            (define edit-enter (new menu-item% [parent edit] [label "Enter command"]
                    [callback (λ source event => focus-on-command-input)]
                    [shortcut-prefix '(meta)] [shortcut #\x]))
            (define edit-eval (new menu-item% [parent edit] [label "Evaluate"]
                    [callback (λ source event => evaluate-current-buffer)]
                    [shortcut-prefix '(ctl)] [shortcut #\e]))
            (append-editor-operation-menu-items edit #t)
        (define navigate (new menu% [parent mb] [label "&Navigate"]))
            (define navigate-quick-open (new menu-item% [parent navigate] [label "&Quick open file"]
                [callback (λ source event => quick-open-file)]
                [shortcut-prefix '(ctl)] [shortcut #\p]))
            (define navigate-go-to-definition (new menu-item% [parent navigate] [label "TODO: Define word under cursor"]
                [callback (λ source event => void)]))
            (define navigate-go-to-line (new menu-item% [parent navigate] [label "TODO: Go to line"]
                [callback (λ source event => void)]))
        (void))
    (install-menu-bar)

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
            )
        )
        (loop this layout)
    )

    ;;  Commands.

    (define/forward     (complete-word-near-cursor) main-editor-area)
    (define/forward     (evaluate-current-buffer) main-editor-area)
    (define/forward     (focus-on-command-input) main-editor-area)
    (define/forward     (focus-on-main-editor) main-editor-area)
    (define/forward     (open-file path) main-editor-area)
    (define/forward     (open-target path position) main-editor-area)
    ;;  Is this the right way to request the application to quit?
    (define/public      (quit) (send frame on-exit))
    (define/forward     (save-file) main-editor-area)
    (define/forward     (set-eval-namespace ns) main-editor-area)
    (define/public      (set-status-message str) (send status-indicator set-label str))
    (define/override    (show bool)
        (when bool (focus-on-main-editor))
        (super show bool))

    (define (quick-open-file) (send* quick-open-dialog [clear] [show #t]))

    ;;  Reactions.

    (define/public (before-open-file buf path) (send this set-label (path->string path)))
    (define/public (after-open-file buf path) (send main-editor-area after-open-file buf path))
    (define/public (after-outline-change buf old new) (send outline-view set-outline new))

    ;;  TODO: Editor tabs, multiple buffers.

    (define/public (open-files paths)
        (for-each (λ path => open-file path) paths)
        #t)
    (define (open-file/ask)
        ;; TODO: dir should be the directory of the current file.
        (define dir #f)
        (define path (get-file #f this dir))
        (when path (open-file path)))
))
