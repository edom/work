#lang s-exp "lang.rkt"

(require
    racket/file
    racket/list
    racket/gui/base
    "core.rkt"
)

(provide
    start-gui
)

(define (start-gui)
    (define frame (new frame% [label "Editor"] [width 800] [height 600]))
        (define mb (new menu-bar% [parent frame]))
        (define m-file (new menu% [parent mb] [label "&File"]))
        (define m-file_save (new menu-item%
            [parent m-file]
            [label "Save"]
            [callback (λ source event ->
                (displayln (send text get-text 0 'eof #t))
            )]
            [shortcut-prefix '(ctl)]
            [shortcut #\S]
        ))
        (define m-file_quit (new menu-item%
            [parent m-file]
            [label "&Quit"]
            [callback (λ source event => send frame on-exit)]
            [shortcut-prefix '(ctl)]
            [shortcut #\Q]
        ))
        (define m-edit (new menu% [parent mb] [label "&Edit"]))
        (define m-edit_complete
            (new menu-item%
                [parent m-edit]
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
        (define m-edit_enter-command
            (new menu-item%
                [parent m-edit]
                [label "Enter command"]
                [callback (λ source event => send state focus-on-command-input)]
                [shortcut-prefix '(meta)]
                [shortcut #\x]
            )
        )
        (define m-font (new menu% [parent mb] [label "F&ont"]))
        (define v-panel (new vertical-panel% [parent frame]))
            (define canvas (new editor-canvas% [parent v-panel]))
                (define handle-key-event (λ e => void))
                (define text (new (class text%
                    (super-new)
                    (define/override (default-style-name) "Code")
                    (define/override (on-default-char e) (handle-key-event e))
                )))
                (send canvas set-editor text)
            (define h-panel-cmd (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
                (define mode-indicator (new message% [parent h-panel-cmd] [label ""] [auto-resize #t]))
                (define command-input (new text-field% [parent h-panel-cmd] [label ""]))
            (define h-panel-sta (new horizontal-panel% [parent v-panel] [stretchable-height #f]))
                (define status-indicator (new message% [parent h-panel-sta] [label ""] [auto-resize #t]))

    (define state (new (class editor-state%
        (super-new)
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
    (set! handle-key-event (λ e => send state handle-key-event e))

    ;;  Call listeners to initialize labels.
    (send state initial-fire-listeners)

    (append-editor-operation-menu-items m-edit #f)
    (append-editor-font-menu-items m-font)

    (initialize-styles text)

    (define path "com-spacetimecat/edit/main.rkt")
    (send text load-file path 'text)
    (send frame set-label (path->string (send text get-filename)))
    (send frame show #t)
    (send canvas focus)
)

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

(define (initialize-styles text)
    (define available-faces (get-face-list 'mono))
    (define preferred-faces (list
        "Noto Mono"
        "Droid Sans Mono"
        "Liberation Mono"
        "DejaVu Sans Mono"
        "Courier New"
        "Monospace"
    ))
    (define face (choose-first
        #:from preferred-faces
        #:in available-faces
        #:or #f
    ))
    ;;  TODO: Set font size.
    (define style-list (send text get-style-list))
    (define style-0 (send style-list basic-style))
    (define delta-0 (new style-delta%))
    (send* delta-0
        (set-face face)
        (set-family 'modern) ;; Racketspeak for monospace.
    )
    (define style-1 (send style-list find-or-create-style style-0 delta-0))
    (send style-list new-named-style "Code" style-1)
    (send text set-styles-sticky #f)
)
