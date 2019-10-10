#lang s-exp "lang.rkt"

(require
    racket/port
)

(require+provide
    (rename-in racket/string
        ;;  string-prefix? looks like an infix operator.
        ;;  string_prefix? looks like a relation.
        (string-prefix? string_prefix?)
        (string-suffix? string_suffix?)
    )
)

(provide
    ->string
)

;;  Convert any Racket value to string.
;;  Unknown values are displayed to a string.
;;
;;  This is like Java's Object.toString.
;;
;;  The string is intended to be read by human programmers while debugging.

(define (->string x)
    ($doc "Convert any Racket value to string.")
    (call-with-output-string
        (lambda (port) (display x port))
    ))
