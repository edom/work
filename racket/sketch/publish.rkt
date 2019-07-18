#lang stc-racket

(define pandoc-bin-path "/home/erik/.local/bin/pandoc")

(define (pandoc)
    (system* pandoc-bin-path "--help")
)
