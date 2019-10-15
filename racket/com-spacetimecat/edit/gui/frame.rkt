#lang s-exp "lang.rkt"

;;  Table of contents:
;;
;;      [DYNASPEC] Dynamic Aspects
;;          [BUFFCMDS] Buffer Commands
;;          [VICONTRO] Vi-like Controls
;;      [STATSPEC] Static Aspects
;;          [STATMENU] Menu Bar

(require
    racket/file
    racket/list
    racket/match
    "key.rkt"
    "outline.rkt"
    "quick-open.rkt"
    "text.rkt"
)

(provide
    my-editor-frame%
)

(define my-editor-frame% (class frame%

    ;;  [DYNASPEC] Dynamic Aspects

    (define (set-status-message str) (send status-indicator set-label str))

    ;;  Intercept all keyboard events that is not handled by the underlying system.
    (define/override (on-subwindow-char r e)
        (define handled-by-parent? (super on-subwindow-char r e))
        (unless handled-by-parent? (handle-key-event e))
        (define hide-event-from-children? #t)
        hide-event-from-children?
    )

    ;;  [BUFFCMDS] Buffer Commands

    (define (move-cursor dir) (send text move-position dir))
    (define (insert-char c) (send text insert (string c)))
    (define (handle-backspace-key) (send text delete 'start 'back))
    (define (handle-delete-key) (send text delete 'start (send text get-end-position)))
    (define (move-to-start-of-next-word) (TODO))
    (define (move-to-previous-start-of-word) (TODO))
    (define (move-to-next-end-of-word) (TODO))

    ;;  [VICONTRO] Vi-like Controls

    (define-property mode 'normal
        #:after-change (old new ->
            (send mode-indicator set-label
                (string-append "-- " (string-upcase (symbol->string new)) " --"))))

    (define (handle-key-event e)
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
                )
            ]
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
                )
            ]
            [else
                (printf "handle-key-event: Invalid mode: ~v~n" mode)
                (set-mode 'normal)
            ]
        )
    )

    (define (focus-on-command-input)
        (set-status-message "TODO: Press Esc to return to main editor area")
        (send command-input focus))
    (define/public (focus-on-main-editor) (send canvas focus))

    (define (open-file/ask)
        (if (send text load-file "" 'text)
            (Let    path (send text get-filename)
            #:in    (after-open-file path)
                    path
            )
            #f
        ))

    ;;  TODO: Ask to save if content is dirty.
    ;;  But what should we do if the user presses "Cancel"?
    ;;  Do nothing? Throw exception? Return #f?
    (define (open-file path)
        (unless (path? path) (error 'open-file "not a path: ~s" path))
        (send text load-file path 'text)
        (after-open-file path))

    (define (open-files paths)
        (for-each open-file paths)
        #t
    )

    (define (after-open-file path)
        (send this set-label (path->string path))
        (send outline-view set-outline (compute-file-outline path)))

    ;;  [STATSPEC] Static Aspects

    (super-new [label "Editor"] [width 800] [height 600])

    (define frame this)

    (define (open-target file position)
        (define current-file (pathy->string (send text get-filename)))
        (define new-file (pathy->string file))
        (when (equal? new-file "")
            (error 'open-target "file path cannot be an empty string: ~s" file))
        (unless (equal? current-file new-file)
            (open-file new-file))
        (send text set-position position position)
        (focus-on-main-editor))

    ;;  Children and layout.

    (define v-panel (new vertical-panel% [parent frame]))
        (define h-panel-main (new horizontal-panel% [parent v-panel]))
            (define outline-view (new outline-view% [parent h-panel-main] [on-request-open open-target]))
            (define canvas (new editor-canvas% [parent h-panel-main]))
                (define text (new my-editor-text%))
                (send canvas set-editor text)
        (define h-panel-cmd (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (define mode-indicator (new message% [parent h-panel-cmd] [label ""] [auto-resize #t]))
            (define command-input (new text-field% [parent h-panel-cmd] [label ""]))
        (define h-panel-sta (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (define status-indicator (new message% [parent h-panel-sta] [label ""] [auto-resize #t]))

    ;;  Dialogs.

    (define quick-open-dialog (new quick-open-dialog% [open-files open-files]))

    ;;  [STATMENU] Menu Bar

    (define (install-menu-bar)
        (define mb (new menu-bar% [parent frame]))
        (define file (new menu% [parent mb] [label "&File"]))
            (define file-open (new menu-item% [parent file]
                [label "&Open"]
                [callback (λ source event => open-file/ask)]
                [shortcut-prefix '(ctl)] [shortcut #\o]
            ))
            (define file-save (new menu-item% [parent file]
                [label "&Save"]
                [callback (λ source event ->
                    (displayln (send text get-text 0 'eof #t))
                )]
                [shortcut-prefix '(ctl)] [shortcut #\s]
            ))
            (define file-quit (new menu-item% [parent file]
                [label "&Quit"]
                [callback (λ source event => send frame on-exit)]
                [shortcut-prefix '(ctl)] [shortcut #\Q]
            ))
        (define edit (new menu% [parent mb] [label "&Edit"]))
            (define edit-complete
                (new menu-item% [parent edit]
                    [label "Complete word near cursor"]
                    [callback (λ source event ->
                        (parameterize [(current-namespace (make-base-namespace))]
                            (define word (get-word-near-cursor text))
                            (define sym-strs (map symbol->string (namespace-mapped-symbols)))
                            (define completions (find-completions #:for word #:in sym-strs))
                            (println (take completions 16))
                        )
                    )]
                    [shortcut-prefix '(ctl)] [shortcut #\space]
                )
            )
            (define edit-enter
                (new menu-item% [parent edit]
                    [label "Enter command"]
                    [callback (λ source event => focus-on-command-input)]
                    [shortcut-prefix '(meta)] [shortcut #\x]
                )
            )
            (append-editor-operation-menu-items edit #f)
        (define navigate (new menu% [parent mb] [label "&Navigate"]))
            (define navigate-quick-open (new menu-item% [parent navigate] [label "&Quick open file"]
                [callback (λ source event ->
                    (send quick-open-dialog clear)
                    (send quick-open-dialog show #t)
                )]
                [shortcut-prefix '(ctl)] [shortcut #\p]
            ))
            (define navigate-go-to-definition (new menu-item% [parent navigate] [label "TODO: Define word under cursor"]
                [callback (λ source event => void)]
            ))
            (define navigate-go-to-line (new menu-item% [parent navigate] [label "TODO: Go to line"]
                [callback (λ source event => void)]
            ))
        (void)
    )
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

    ;;  Call listeners to initialize labels.
    (define (initial-fire-listeners)
        (before-mode-change mode mode)
        (after-mode-change mode mode))
    (initial-fire-listeners)
))

(define-syntax (loop stx)
    (syntax-parse stx
        [   (_ #:to-break-call break body ...)
            #'(call/cc (λ break ->
                (define (loop) body ... (loop))
                (loop)
            ))
        ]
    ))

(define (extend-selection-to-nearest-word-boundaries text)
    (define start (send text get-start-position))
    (define end (send text get-end-position))
    (define min-start 0)
    (define max-end (send text last-position))
    (define word-breaking-chars '(
        #\nul #\space #\tab #\newline #\return
        #\( #\) #\[ #\] #\{ #\}
    ))
    (define (char-breaks-word? c) (member c word-breaking-chars))
    (loop #:to-break-call break
        (when (<= start min-start) (break))
        (define char (send text get-character (- start 1)))
        (when (char-breaks-word? char) (break))
        (set! start (- start 1))
    )
    (loop #:to-break-call break
        (when (>= end max-end) (break))
        (define char (send text get-character end))
        (when (char-breaks-word? char) (break))
        (set! end (+ end 1))
    )
    (values start end)
)

(define (get-word-near-cursor text)
    (define-values (start end)
        (extend-selection-to-nearest-word-boundaries text)
    )
    (send text get-text start end)
)
