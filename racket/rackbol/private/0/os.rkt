#lang s-exp "_in.rkt"

(provide
    system**/exit-code
)

(define-syntax-rule (system**/exit-code Path Arg ...)
    (system*/exit-code
        Path
        (string->bytes/utf-8 (->string Arg))
        ...))
