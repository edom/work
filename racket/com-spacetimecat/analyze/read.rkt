#lang s-exp "lang.rkt"

(require
    (prefix-in u: "read-unrestricted.rkt")
    "restrict.rkt"
)

(provide
    read-module-from-file
    make-namespace-from-file
)

;;  --------------------    Read.

(define (read-module-from-file path)
    (begin-with-restrictions
        (u:read-module-from-file path)))

;;  --------------------    Parse.

(define (make-namespace-from-file path)
    (begin-with-restrictions
        (u:make-namespace-from-file path)))
