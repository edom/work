#lang racket/base

(require
    racket/format
    "buffer.rkt"
    "x86-64.rkt"
)

(provide (all-defined-out))

(define (displayptrln ptr)
    (displayln (format-ptr ptr)))

;;  --------------------    Test.

(define (main)
    (define limit (get-page-size))
    (define buffer (allocate-buffer limit))

    (define ptr (Buffer-ptr buffer))
    (printf "Allocated ~a bytes of executable memory at ~a~n" limit (format-ptr ptr))

    (define assembly
        `[
            (mov rax #x0123456789abcdef)
            (ret)
        ])
    (assemble! buffer assembly)
    (Buffer-dump buffer #:row-size 64)

    (displayln (~r #:base 16 (Buffer-call buffer (_fun -> _uint64))))
    (displayln "Still alive?")

    (free-buffer! buffer)
    (printf "Freed ~a bytes of executable memory at ~a~n" limit (format-ptr ptr))
)
