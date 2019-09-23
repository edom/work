#lang racket

(module reader syntax/module-reader rackbol)

(require "private/main.rkt")

(provide

    pretty-print

    include

    λ
    LET
    LET*
    LETREC
    LETREC*
    GROUP

    DEFINE

    psql

    DEFINITIONS

)

(provide [except-out (all-from-out racket) #%module-begin])
(provide [rename-out (module_begin #%module-begin)])

(define-syntax-rule (module_begin Body ...)
    (#%module-begin Body ... (GENERATE))
)
