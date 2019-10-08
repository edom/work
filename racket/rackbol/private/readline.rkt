#lang s-exp "racket-base.rkt"

;;  2019-10-05:
;;  Problem: Racket's clobbering stdin confuses psql.
;;  Solution: Use with-original-stdin.
;;
;;  We want the readline stdin for Racket REPL,
;;  but we want the terminal stdin for subprocesses,
;;  because interactive subprocesses such as psql
;;  expect that stdin is a terminal.

(require
    (for-syntax racket/base)
    readline
)

(provide
    get-original-stdin
    with-original-stdin
)

(define (get-original-stdin)
    (or pre-readline-input-port (current-input-port)))

(define-syntax-rule (with-original-stdin Body ...)
    (parameterize ([current-input-port (get-original-stdin)])
        Body ...))
