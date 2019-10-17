#lang s-exp "lang.rkt"

(require
    racket/list
    racket/match
    "../eval.rkt"
    "key.rkt"
    "text.rkt"
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

    (define text (new my-editor-text% [control control]))
    (send canvas set-editor text)

    ;;  Eval.

    (define namespace (current-namespace))
    (define/public (set-eval-namespace ns) (set! namespace ns))
    ;;  The REPL runs on the GUI thread, so the GUI will hang until eval returns.
    (define/public (evaluate-current-buffer)
        (define input-str (send text get-text))
        (define output-str (eval/string input-str namespace))
        (send text append (string-append "\n\n" output-str)))

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

    ;;  Translate user intentions.

    (define (move-cursor dir) (send text move-position dir))
    (define (insert-char c) (send text insert (string c)))
    (define (handle-backspace-key) (send text delete 'start 'back))
    (define (handle-delete-key) (send text delete 'start (send text get-end-position)))
    (define (move-to-start-of-next-word) (TODO))
    (define (move-to-previous-start-of-word) (TODO))
    (define (move-to-next-end-of-word) (TODO))

    (define/forward (open-file path) text)
    (define/forward (open-target path position) text)
    (define/forward (save-file) text)
    (define/forward (complete-word-near-cursor) text)

    (define/public (focus-on-command-input) (send command-input focus))
    (define/public (focus-on-main-editor) (send canvas focus))

    ;;  Call listeners to initialize labels.
    (define (initial-fire-listeners)
        (before-mode-change mode mode)
        (after-mode-change mode mode))
    (initial-fire-listeners)
))
