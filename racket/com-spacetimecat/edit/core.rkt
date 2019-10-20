#lang s-exp "lang.rkt"

(require+provide
    "../analyze/all.rkt"
    "complete.rkt"
)

(provide

    parse-shortcut

    choose-first

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
    (define assocs (map (λ key -> (cons key #t)) available))
    (define table (make-hash assocs))
    (define prefs (filter (λ pref -> (hash-ref table pref #f)) preferences))
    (if (null? prefs)
        fallback
        (car prefs)
    ))

(require
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
