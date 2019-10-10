#lang s-exp "lang.rkt"

(require "maintain.rkt")

(provide main)

(define (main)
    (analyze-dependencies "com-spacetimecat/rackbol/lang.rkt")
)
