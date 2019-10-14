#lang s-exp "lang.rkt"

(require
    racket/port
)

(require+provide
    (except-in racket/string
        string-prefix?
        string-suffix?
    )
    (rename-in racket/string
        ;;  string-prefix? looks like an infix operator.
        ;;  string_prefix? looks like a relation.
        (string-prefix? string_prefix?)
        (string-suffix? string_suffix?)
    )
)

(provide
    ->string
    string-last-index-of
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

;;  Let n be the return value.
;;  It satisfies this property:
;;      If n is a number, then
;;          (char=? (string-ref str n) char).

(define/contract (string-last-index-of str char)
    (-> string? char? (or/c exact-nonnegative-integer? #f))
    (define (loop i)
        (if (>= i 0)
            (if (char=? (string-ref str i) char)
                i
                (loop (- i 1))
            )
            #f
        )
    )
    (loop (- (string-length str) 1))
)
