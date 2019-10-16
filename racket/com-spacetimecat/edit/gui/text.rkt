#lang s-exp "lang.rkt"

(provide
    my-editor-text%
)

(define preferred-faces (list
    "Noto Mono"
    "Droid Sans Mono"
    "Liberation Mono"
    "DejaVu Sans Mono"
    "Courier New"
    "Monospace"
))

;;  TODO: Rename to buffer%
;;  Each instance of this class is intended to correspond to one open file.

(define my-editor-text% (class text%
    (init-field control)
    (super-new)

    (define/override (on-event e)
        (define admin (send this get-admin))
        (when (and admin (send e button-down? 'right))
            (send admin popup-menu (make-context-menu) (send e get-x) (send e get-y))
        ))

    (define (make-context-menu)
        (define word (get-word-near-cursor this))
        (define menu (new popup-menu%))
        (define-syntax-rule (f arg ...) (~a arg ... #:separator " "))
        (new menu-item% [parent menu] [label (f "Word near cursor:" word)] [callback (λ r e => void)])
        (new menu-item% [parent menu] [label (f "TODO: Define" word)] [callback (λ r e => void)])
        (new menu-item% [parent menu] [label "TODO: Open <module>"] [callback (λ r e => void)])
        (new menu-item% [parent menu] [label (f "TODO: Show documentation for" word)] [callback (λ r e => void)])
        (new menu-item% [parent menu] [label "TODO: Find what uses <word/module>"] [callback (λ r e => void)])
        (new menu-item% [parent menu] [label "TODO: Find what is used by <word/module>"] [callback (λ r e => void)])
        (new menu-item% [parent menu] [label "TODO: Show <word/module> dependencies/uses/used-by"] [callback (λ r e => void)])
        menu)

    (define-property outline #f
        #:after-change (old new ->
            (send control after-outline-change this old new)
        ))

    ;;  Problem: load-file should be private.
    ;;  TODO: Ask to save if content is dirty.
    ;;  But what should we do if the user presses "Cancel"?
    ;;  Do nothing? Throw exception? Return #f?
    (define/override (load-file path)
        (unless (path? path) (error 'load-file "not a path: ~s" path))
        (super load-file path 'text))

    ;;  This is what open-file does after calling load-file.
    ;;  This is called by open-file and its variants, not by load-file.
    (define (after-load-file path)
        (set-outline (compute-file-outline path))
        (send control after-open-file this path))

    ;;  open = load + parse + fire events
    ;;
    ;;  2019-10-16: Design Question:
    ;;  Should open-path accept relative paths?
    ;;  If yes, what should it be relative to?
    ;;  The working directory? The project directory?

    (define/public (get-file-path)
        (define pathy (send this get-filename))
        (if (path? pathy) pathy (string->path pathy)))

    (define/public (open-file path)
        (send control before-open-file this path)
        (load-file path)
        (after-load-file path))

    (define/public (open-target target-path position)
        (define current-path (get-file-path))
        (define target-str (path->string target-path))
        (when (equal? target-str "")
            (error 'open-target "target path cannot be empty: ~s" target-path))
        (unless (equal? current-path target-path)
            (open-file target-path))
        (send this set-position position position))

    ;;  Styles.

    (define/override (default-style-name) "Code")

    (define (initialize-styles)
        (define available-faces (get-face-list 'mono))
        (define face (choose-first
            #:from preferred-faces
            #:in available-faces
            #:or #f
        ))
        ;;  TODO: Set font size.
        (define style-list (send this get-style-list))
        (define style-0 (send style-list basic-style))
        (define delta-0 (new style-delta%))
        (send delta-0 set-face face)
        (send delta-0 set-family 'modern) ;; Racketspeak for monospace.
        (define style-1 (send style-list find-or-create-style style-0 delta-0))
        (send style-list new-named-style "Code" style-1)
        (send this set-styles-sticky #f)
    )

    (initialize-styles)
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
        (set! start (- start 1)))
    (loop #:to-break-call break
        (when (>= end max-end) (break))
        (define char (send text get-character end))
        (when (char-breaks-word? char) (break))
        (set! end (+ end 1)))
    (values start end))

(define (get-word-near-cursor text)
    (define-values (start end)
        (extend-selection-to-nearest-word-boundaries text))
    (send text get-text start end))
