#lang s-exp "lang.rkt"

(require
    racket/file
    racket/list
    "outline.rkt"
    "text.rkt"
    "quick-open.rkt"
)

(provide
    my-editor-frame%
)

(define my-editor-frame% (class frame%

    (super-new [label "Editor"] [width 800] [height 600])

    (define frame this)

    (define (path-like->string x)
        (cond
            [(path? x) (path->string x)]
            [(string? x) x]
        )
    )

    ;;  --------------------    Children and layout.

    (define v-panel (new vertical-panel% [parent frame]))
        (define h-panel-main (new horizontal-panel% [parent v-panel]))
            (define outline-view (new
                (class outline-view% (super-new)
                    (define/override (open-target file position)
                        ;;  TODO open file
                        (define current-file (path-like->string (send text get-filename)))
                        (define new-file (path-like->string file))
                        (when (equal? new-file "")
                            (error 'open-target "file path cannot be an empty string: ~s" file))
                        (unless (equal? current-file new-file)
                            (load-file new-file))
                        (send text set-position position position)
                        (send frame focus-on-main-editor)
                    )
                )
                [parent h-panel-main]
            ))
            (define canvas (new editor-canvas% [parent h-panel-main]))
                (define text (new (class my-editor-text% (super-new)
                    (define/override (on-default-char e) (handle-key-event e))
                )))
                (send canvas set-editor text)
        (define h-panel-cmd (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (define mode-indicator (new message% [parent h-panel-cmd] [label ""] [auto-resize #t]))
            (define command-input (new text-field% [parent h-panel-cmd] [label ""]))
        (define h-panel-sta (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
            (define status-indicator (new message% [parent h-panel-sta] [label ""] [auto-resize #t]))

    ;;  --------------------    Dialogs.

    (define quick-open-dialog
        (new (class quick-open-dialog% (super-new)
                (define/override (open-file path)
                    (load-file path)
                )
            )
            [parent this]
        ))

    ;;  --------------------    Menu bar.

    (define (install-menu-bar)
        (define mb (new menu-bar% [parent frame]))
        (define file (new menu% [parent mb] [label "&File"]))
        (define file-open (new menu-item%
            [parent file]
            [label "&Open"]
            [callback (λ source event => ask-open-file)]
            [shortcut-prefix '(ctl)]
            [shortcut #\o]
        ))
        (define file-quick-open (new menu-item%
            [parent file]
            [label "&Quick Open"]
            [callback (λ source event ->
                (send quick-open-dialog clear)
                (send quick-open-dialog show #t)
            )]
            [shortcut-prefix '(ctl)]
            [shortcut #\p]
        ))
        (define file-save (new menu-item%
            [parent file]
            [label "&Save"]
            [callback (λ source event ->
                (displayln (send text get-text 0 'eof #t))
            )]
            [shortcut-prefix '(ctl)]
            [shortcut #\s]
        ))
        (define file-quit (new menu-item%
            [parent file]
            [label "&Quit"]
            [callback (λ source event => send frame on-exit)]
            [shortcut-prefix '(ctl)]
            [shortcut #\Q]
        ))
        (define edit (new menu% [parent mb] [label "&Edit"]))
        (define edit-complete
            (new menu-item%
                [parent edit]
                [label "Complete word near cursor"]
                [callback (λ source event ->
                    (parameterize [(current-namespace (make-base-namespace))]
                        (define word (get-word-near-cursor text))
                        (define sym-strs (map symbol->string (namespace-mapped-symbols)))
                        (define completions (find-completions #:for word #:in sym-strs))
                        (println (take completions 16))
                    )
                )]
                [shortcut-prefix '(ctl)]
                [shortcut #\space]
            )
        )
        (define edit-enter
            (new menu-item%
                [parent edit]
                [label "Enter command"]
                [callback (λ source event => send state focus-on-command-input)]
                [shortcut-prefix '(meta)]
                [shortcut #\x]
            )
        )
        (define font (new menu% [parent mb] [label "F&ont"]))
        (append-editor-operation-menu-items edit #f)
        (append-editor-font-menu-items font)
    )
    (install-menu-bar)

    (define state (new (class editor-state% (super-new)
        (define/override (set-status-message str) (send status-indicator set-label str))
        (define/override (focus-on-command-input)
            (send command-input focus)
            (super focus-on-command-input)
        )
        (define/override (move-cursor dir) (send text move-position dir))
        (define/override (insert-char c) (send text insert (string c)))
        (define/override (handle-backspace-key) (send text delete 'start 'back))
        (define/override (handle-delete-key) (send text delete 'start (send text get-end-position)))
        (define/override (after-mode-change old new)
            (send mode-indicator set-label
                (string-append "-- " (string-upcase (symbol->string new)) " --")
        ))
    )))

    ;;  The final wire that completes the cycle in the object graph.
    (define (handle-key-event e) (send state handle-key-event e))

    ;;  Call listeners to initialize labels.
    (send state initial-fire-listeners)

    ;;  --------------------    End initialization.

    ;;  There seems to be some duplication of work in load-file.

    (define (ask-open-file)
        (if (send text load-file "" 'text)
            (Let    path (send text get-filename)
            #:in    (after-load-file path)
                    path
            )
            #f
        )
    )

    ;;  TODO: Ask to save if content is dirty.
    ;;  But what should we do if the user presses "Cancel"?
    ;;  Do nothing? Throw exception? Return #f?
    (define/public (load-file path)
        (unless (path? path)
            (error 'load-file "not a path: ~s" path))
        (send text load-file path 'text)
        (after-load-file path)
    )

    (define (after-load-file path)
        (send this set-label (path->string path))
        (send outline-view set-outline (compute-file-outline path))
    )

    (define/public (focus-on-main-editor)
        (send canvas focus)
    )
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
