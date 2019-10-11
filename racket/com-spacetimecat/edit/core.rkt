#lang s-exp "lang.rkt"

(require
    racket/list
    racket/match
)

(provide

    editor-state%

    order-strings
    find-completions

    parse-shortcut

    choose-first

)

(define editor-state% (class object%
    (super-new)

    (define-property mode 'normal)

    ;;  --------------------    Primitives that should be overridden.
    ;;
    ;;  These should be wired to the GUI actuators.

    (define/public (set-status-message str) (void))
    (define/public (focus-on-main-editor) (void))
    (define/public (focus-on-command-input) (set-status-message "TODO: Press Esc to return to main editor area"))
    (define/public (move-cursor dir) (void))
    (define/public (insert-char c) (void))
    (define/public (handle-backspace-key) (void))
    (define/public (handle-delete-key) (void))

    ;;  --------------------    Primitives that may be overridden.

    (define/public (default-handle-key-event e)
        (printf "DEBUG: Unhandled key event in normal mode: ~a~n" (format-key-event e))
    )

    (define (format-key-event e)
        (define key (send e get-key-code))
        (~a
            (if (send e get-control-down) "C-" "")
            (if (send e get-alt-down) "M-" "")
            (if (send e get-shift-down) "S-" "")
            key
            (if (send e get-caps-down) " (caps lock on)" "")
        )
    )

    ;;  --------------------    Provisions that should not be overridden.

    (define/public (initial-fire-listeners)
        (before-mode-change mode mode)
        (after-mode-change mode mode)
    )

    (define (key-event->combination e)
        (define key (send e get-key-code))
        (define alt (send e get-alt-down))
        (define ctrl (send e get-control-down))
        (append
            (if ctrl '(ctrl) '())
            (if alt '(alt) '())
            (list key)
        )
    )

    ;;  Translate key event into action.

    (define/public (handle-key-event e)
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
                    [else (default-handle-key-event e)]
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
                    [_ (default-handle-key-event e)]
                )
            ]
            [else
                (printf "handle-key-event: Invalid mode: ~v~n" mode)
                (set-mode 'normal)
            ]
        )
    )
))

(define
    (order-strings
        #:in candidates
        #:according-to user-input
    )
    (define (order-of x)
        (cond
            [(equal? x user-input) 0]
            [(string_prefix? x user-input) 1]
            [else 2]
        ))
    (define (comes-before? x y)
        (if (< (order-of x) (order-of y))
            #t
            #f
        )
    )
    (sort candidates comes-before?)
)

(define
    (find-completions
        #:for input
        #:in strings
    )
    (order-strings #:in strings #:according-to input)
)

;;  input is a symbol or a string.
;;
;;  The intention is to translate 'C-M-s to '(ctl alt s)
;;  or '(ctl meta s) or '(cmd opt s) depending on system type.
;;
;;  See also: Emacs's kbd procedure.

(define (parse-shortcut input)
    (define str (->string input))
    (define Ctrl 'ctl)
    (define Shift 'shift)
    (define Meta 'meta)
    (define-values (ctrl meta key-str)
        (match (string-split str "-")
            [`("C" ,key) (values #t #f key)]
            [`("C" "M" ,key) (values #t #t key)]
            [`("M" ,key) (values #f #t key)]
            [_ (error 'parse-shortcut "invalid key binding: ~v" input)]
        ))
    (when (!= (string-length key-str) 1)
        (error 'parse-shortcut "invalid key binding: ~v" input)
    )
    (define key-char (string-ref key-str 0))
    (define-values (shift key)
        (if (char-upper-case? key-char)
            (values #t (char-downcase key-char))
            (values #f key-char)
        )
    )
    (define (bool b e) (if b (list e) '()))
    (cons
        (append
            (bool ctrl Ctrl)
            (bool shift Shift)
            (bool meta Meta)
        )
        key
    )
)

(define (choose-first
        #:from preferences
        #:in available
        #:or (fallback #f)
    )
    (define assocs (map (λ key => cons key #t) available))
    (define table (make-hash assocs))
    (define prefs (filter (λ pref => hash-ref table pref #f) preferences))
    (if (null? prefs)
        fallback
        (car prefs)
    ))
