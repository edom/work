#lang s-exp "lang.rkt"

(require
    racket/list
    "buffer.rkt"
    "key.rkt"
)

(provide
    main-editor-area%
)

(define main-editor-area% (class tab-panel%
    (super-new  [choices '()]
                [stretchable-width #t]
                [stretchable-height #t])
    (init-field control)

    (define canvas (new editor-canvas% [parent this]))
    (define h-panel-cmd (new horizontal-panel% [parent this] [stretchable-height #f]))
        (define mode-indicator (new message% [parent h-panel-cmd] [label ""] [auto-resize #t]))
        (define command-input (new text-field% [parent h-panel-cmd] [label ""]))

    (define buffer (new buffer% [control control]))
    (send canvas set-editor (send buffer internal-get-text%-instance))

    (define/public (evaluate-current-buffer) (send buffer evaluate))

    ;;  Keyboard event handling.

    ;;  Should we use the keymap that comes with racket/gui?
    (define keymap 'normal)
    (define/override (on-subwindow-char r e)
        (or (super on-subwindow-char r e)
            (and (equal? keymap 'vi) (handle-key-event e))))

    (define/public (handle-key-event e)
        ;;  What about things like 10dw, d10w, 5h
        ;;  We need a more sophisticated parser.
        (define handled? #t)
        (define (unhandled)
            (set! handled? #f)
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
                (set-mode 'normal)])
        handled?)

    (define-property mode 'normal
        #:after-change (old new ->
            (send mode-indicator set-label
                (string-append "-- " (string-upcase (symbol->string new)) " --"))))

    ;;  Reactions.

    (define/public (after-open-file buf path)
        ;;  TODO: Use the short file name instead of the complete path.
        ;;  TODO: Disambiguate between files with the same name
        ;;  by showing the rightmost unambiguous subdirectory.
        (send this append (path->string path))
        (focus-on-main-editor))

    (define/forward (move-cursor dir)                   buffer)
    (define/forward (insert-char c)                     buffer)
    (define/forward (handle-backspace-key)              buffer)
    (define/forward (handle-delete-key)                 buffer)
    (define/forward (move-to-start-of-next-word)        buffer)
    (define/forward (move-to-previous-start-of-word)    buffer)
    (define/forward (move-to-next-end-of-word)          buffer)

    (define/forward (open-file path)                    buffer)
    (define/forward (open-target path position)         buffer)
    (define/forward (save-file)                         buffer)
    (define/forward (complete-word-near-cursor)         buffer)

    (define/public (focus-on-command-input) (send command-input focus))
    (define/public (focus-on-main-editor) (send canvas focus))

    ;;  Call listeners to initialize labels.
    (define (initial-fire-listeners)
        (before-mode-change mode mode)
        (after-mode-change mode mode))
    (initial-fire-listeners)
))
