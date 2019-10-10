#lang s-exp "lang.rkt"

(require
    "string.rkt"
)

(require+provide
    racket/system
)

(provide
    system**/exit-code
    with-environment
)

;;  --------------------    Operating system.

;;  This is like system*/exit-code but this automatically converts all strings to bytes with UTF-8 encoding.

(define (system**/exit-code path . args)
    (define (->bytes x) (string->bytes/utf-8 (->string x)))
    (apply system*/exit-code path (map ->bytes args)))

;;  This provides let-like syntax for parameterize-ing current-environment-variables.
;;  Keys are automatically quoted.
;;  Everything is converted to string.
;;  Example:
;;      (with-environment ([foo 'bar] [baz 1]) (displayln 'hello))

(define-syntax-rule (with-environment ((Key Value) ...) Thunk ...)
    (let ((env (make-environment-variables)))
        (environment-variables-set! env
            (string->bytes/utf-8 (->string 'Key))
            (string->bytes/utf-8 (->string Value))
        ) ...
        (parameterize ((current-environment-variables env)) Thunk ...)))
