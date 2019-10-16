#lang s-exp "lang.rkt"

(require
    racket/file
    racket/list
    racket/match
    "key.rkt"
    "outline.rkt"
    "quick-open.rkt"
    "text.rkt"
)

;;  basic-editor-frame% focuses on the appearance.
;;  behaving-editor-frame% focuses on the behavior.
;;  2019-10-16: Design Question:
;;  Is that the best way to decompose the problem for maintainability?

(provide
    basic-editor-frame%
    behaving-editor-frame%
)

(define basic-editor-frame% (class frame%

    (super-new [label "Editor"] [width 800] [height 600])

    (define frame this)

    ;;  Static aspects: children, layout, dialogs, and menu bar.

    (define v-panel (new vertical-panel% [parent frame]))
        (define h-panel-main (new horizontal-panel% [parent v-panel]))
            (define outline-view (new outline-view% [parent h-panel-main] [control this]))
            (field [canvas (new editor-canvas% [parent h-panel-main])])
                (field [text (new my-editor-text% [control frame])])
                (send canvas set-editor text)
        (define h-panel-cmd (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (field [mode-indicator (new message% [parent h-panel-cmd] [label ""] [auto-resize #t])])
            (field [command-input (new text-field% [parent h-panel-cmd] [label ""])])
        (define h-panel-sta (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (define status-indicator (new message% [parent h-panel-sta] [label ""] [auto-resize #t]))

    (define quick-open-dialog (new quick-open-dialog% [control this]))

    ;;  The menu should not be too big.
    ;;  Features should be disclosed gradually.
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
                [callback (λ source event => send frame on-exit)]
                [shortcut-prefix '(ctl)] [shortcut #\Q]))
        (define edit (new menu% [parent mb] [label "&Edit"]))
            (define edit-complete (new menu-item% [parent edit] [label "Complete word near cursor"]
                    [callback (λ source event => complete-word-near-cursor)]
                    [shortcut-prefix '(ctl)] [shortcut #\space]))
            (define edit-enter (new menu-item% [parent edit] [label "Enter command"]
                    [callback (λ source event => focus-on-command-input)]
                    [shortcut-prefix '(meta)] [shortcut #\x]))
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

    (abstract open-file/ask)
    (define/public (focus-on-command-input) (send command-input focus))
    (define/public (focus-on-main-editor) (send canvas focus))
    (define/public (set-status-message str) (send status-indicator set-label str))
    (define/forward (open-target path position) text)
    (define (quick-open-file) (send* quick-open-dialog [clear] [show #t]))
    (define (save-file)
        (displayln (send text get-text 0 'eof #t))
        (TODO))
    (define (complete-word-near-cursor)
        (parameterize [(current-namespace (make-base-namespace))]
            (define word (send text get-word-near-cursor))
            (define sym-strs (map symbol->string (namespace-mapped-symbols)))
            (define completions (find-completions #:for word #:in sym-strs))
            (println (take completions 16))
        ))

    ;;  Reactions.

    (define/public (before-open-file buf path) (send this set-label (path->string path)))
    (define/public (after-open-file buf path) (focus-on-main-editor))
    (define/public (after-outline-change buf old new) (send outline-view set-outline new))
))

(define behaving-editor-frame% (class basic-editor-frame% (super-new)
    (inherit-field
        command-input
        canvas
        mode-indicator
        text
    )
    (inherit
        focus-on-command-input
        focus-on-main-editor
        set-status-message
    )

    (define/override (show bool)
        (when bool (focus-on-main-editor))
        (super show bool))

    ;;  Intercept all keyboard events that is not handled by the underlying system.
    (define/override (on-subwindow-char r e)
        (define handled-by-parent? (super on-subwindow-char r e))
        (unless handled-by-parent? (handle-key-event e))
        (define hide-event-from-children? #t)
        hide-event-from-children?
    )

    (define-property mode 'normal
        #:after-change (old new ->
            (send mode-indicator set-label
                (string-append "-- " (string-upcase (symbol->string new)) " --"))))

    (define/public (handle-key-event e)
        ;;  What about things like 10dw, d10w, 5h
        ;;  We need a more sophisticated parser.
        (define (unhandled)
            (printf "DEBUG: Unhandled key event in normal mode: ~a~n" (format-key-event e)))
        (define combination (key-event->combination e))
        (case mode
            [(normal)
                (match combination
                    [`(#\i) (set-mode 'insert)]
                    [`(#\:) (focus-on-command-input)]
                    [(list (or #\h 'left)) (move-cursor 'left)]
                    [(list (or #\j 'down)) (move-cursor 'down)]
                    [(list (or #\k 'up)) (move-cursor 'up)]
                    [(list (or #\l 'right)) (move-cursor 'right)]
                    [(or '(#\w) '(ctl right)) (move-to-start-of-next-word)]
                    [(or '(#\b) '(ctl left)) (move-to-previous-start-of-word)]
                    ['(#\e) (move-to-next-end-of-word)]
                    [else (unhandled)]
                )]
            [(insert)
                (match combination
                    [`(escape) (set-mode 'normal)]
                    [`(,c) #:when (char? c)
                        (case c
                            [(#\backspace) (handle-backspace-key)]
                            [(#\rubout) (handle-delete-key)]
                            [(#\return) (insert-char #\newline)]
                            [else (insert-char c)]
                        )
                    ]
                    [`(left) (move-cursor 'left)]
                    [`(down) (move-cursor 'down)]
                    [`(up) (move-cursor 'up)]
                    [`(right) (move-cursor 'right)]
                    [_ (unhandled)]
                )]
            [else
                (printf "handle-key-event: Invalid mode: ~v~n" mode)
                (set-mode 'normal)]))

    ;;  Translate user intentions.

    (define (move-cursor dir) (send text move-position dir))
    (define (insert-char c) (send text insert (string c)))
    (define (handle-backspace-key) (send text delete 'start 'back))
    (define (handle-delete-key) (send text delete 'start (send text get-end-position)))
    (define (move-to-start-of-next-word) (TODO))
    (define (move-to-previous-start-of-word) (TODO))
    (define (move-to-next-end-of-word) (TODO))

    ;;  TODO: Editor tabs, multiple buffers.

    (define (open-file path) (send text open-file path))
    (define/public (open-files paths) (for-each open-file paths) #t)
    (define/override (open-file/ask)
        ;; TODO: dir should be the directory of the current file.
        (define dir #f)
        (define path (get-file #f this dir))
        (when path (send text open-file path)))

    ;;  Call listeners to initialize labels.
    (define (initial-fire-listeners)
        (before-mode-change mode mode)
        (after-mode-change mode mode))
    (initial-fire-listeners)
))
