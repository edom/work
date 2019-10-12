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

    ;;  Outline.

    outline-node/c
    outline-node-list/c

    compute-file-outline

    outline-node-type
    outline-node-target
    outline-node-children

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
    ;;  TODO
    (define/public (move-to-start-of-next-word) (void))
    (define/public (move-to-previous-start-of-word) (void))
    (define/public (move-to-next-end-of-word) (void))

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
        ;;  What about things like 10dw, d10w, 5h
        ;;  We need a more sophisticated parser.
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

;;  See compute-module-outline for the return value.

(define (compute-file-outline path)
    (define mod
        (parameterize (
                (read-accept-lang #t)
                (read-accept-reader #t)
            )
            (read-syntax-from-file path)
        ))
    (compute-module-outline mod)
)

;;  The shape of the return value is Parts:
;;
;;      Parts       ::= (Part ...)
;;      Part        ::= Type Children
;;      Type        ::= (module Id Init)
;;                  |   (require Spec)
;;                  |   (provide Spec)
;;                  |   (variable Id)
;;                  |   (procedure Id)
;;                  |   (parameter Id)
;;                  |   (class Super)
;;      Children    ::= Parts
;;
;;  This is only an approximation.
;;  This is the 20% effort that works for 80% Racket programs.
;;
;;  We don't use #:literals because it requires computing the bindings
;;  which requires fully-expanding the module.

(define outline-type/c any/c)
(define outline-target/c (or/c syntax? #f))
(define outline-node/c (list/c
    outline-type/c
    outline-target/c
    [recursive-contract outline-node-list/c]
))
(define outline-node-list/c (listof [recursive-contract outline-node/c]))

(define (outline-node-type x) (list-ref x 0))
(define (outline-node-target x) (list-ref x 1))
(define (outline-node-children x) (list-ref x 2))

(define/contract (compute-module-outline stx)
    (-> syntax? outline-node-list/c)
    (define (d stx) (syntax->datum stx))
    (define (loop stx)
        ;;  Each case produces a list of parts.
        (syntax-parse stx
            #:datum-literals (
                module #%module-begin require provide
                begin begin-for-syntax
                class class*
            )
            [   (module Id:id Init (#%module-begin Body ...))
                `([ (module ,(d #'Id) ,(d #'Init))
                    ,#'Id
                    ,(loop #'(begin Body ...))
                ])
            ]
            [   (begin)
                '()
            ]
            [   (begin Body1 Body2 ...)
                `(
                    ,@(loop #'Body1)
                    ,@(loop #'(begin Body2 ...))
                )
            ]
            [   (begin-for-syntax Body ...)
                (loop #'(begin Body ...))
            ]
            [   (require . Specs) (map (λ s -> `([require ,(d s)] ,s [])) (syntax->list #'Specs))]
            [   (provide . Specs) (map (λ s -> `([provide ,(d s)] ,s [])) (syntax->list #'Specs))]
            ;;  TODO: lambda
            [   (_:define$ (Id:id . Params) Body ...)
                `([ (procedure ,(d #'Id))
                    ,#'Id
                    (   ,@(map (λ p -> `([parameter ,(d p)] ,p [])) (syntax->list #'Params))
                        ,@(loop #'(begin Body ...))
                    )
                ])
            ]
            [   (_:define$ Id:id Init)
                `([ (variable ,(d #'Id))
                    ,#'Id
                    ,(loop #'Init)
                ])
            ]
            [   (class Super:expr Body ...)
                `([ (class ,(d #'Super))
                    ,#'Super
                    ,(loop #'(begin Body ...))
                ])
            ]
            [   (class* Super:expr _Ifaces:expr Body ...)
                `([ (class ,(d #'Super))
                    ,#'Super
                    ,(loop #'(begin Body ...))
                ])
            ]
            [   (Expr ...) (loop #'(begin Expr ...))]
            [   _ '()]
        )
    )
    (loop stx)
)

(require
    syntax/parse
    syntax/srcloc
)

(define (syntax-srcloc stx)
    (make-srcloc
        (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)
    ))

(define-syntax-class define$
    #:datum-literals (
        define
        define-syntax
        define-for-syntax
        define/public
        define/pubment
        define/augment
        define/override
    )
    [pattern
        (~or*
            define
            define-syntax
            define-for-syntax
            define/public
            define/pubment
            define/augment
            define/override
        )
    ]
)
