#lang racket

(module Generator racket
    (provide (rename-out [module_begin #%module-begin]))
    (require "unhygienic.rkt")
    (define-syntax (module_begin stx)
        (syntax-case stx ()
            [(_ Arg ...)
                #`(#%module-begin
                    (require "unhygienic.rkt")
                    (provide (all-defined-out))
                    (define x 100)
                    ;;  Arg ... can see x.
                    (UNHYGIENIC Arg ...)
                )
            ]))
)

(module Client (submod ".." Generator)
    ;;  x comes unhygienically.
    (define y x)
    y
)

(require (submod "." Client))
(provide (all-from-out (submod "." Client)))
