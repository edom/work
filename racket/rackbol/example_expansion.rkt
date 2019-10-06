#lang racket

(define-syntax-rule (define-syntax-case (Id Param ...) Body ...)
    (define-syntax (Id stx)
        (syntax-case stx ()
            ([_ Param ...] Body ...))))

;;  No clash. DEFA defines a unique symbol every time it expands.
(define a 1)
(define-syntax (DEFA _) #'(begin (define a 2) (print a) (newline)))
(define-syntax (DEFB stx)
    (syntax-case stx ()
        [(_ Id)
            #`(define #,(datum->syntax #'Id 'b) 0)
        ]
    ))
(DEFA)
(DEFA)
(if #t (DEFB a) 0)

(module Model racket
    (provide (all-defined-out))
    (struct Var1 (Id Init) #:prefab)
    (define VARS '())
    (define (add_var! x) (set! VARS (cons x VARS)))
)
(module Macro racket
    (provide DEFVAR GENERATE)
    (require (for-syntax (submod ".." Model)))
    (require (for-syntax syntax/strip-context))

    (define-syntax-case (DEFVAR Id Init)
        (add_var! (Var1 #'Id #'Init))
        ;#'(define Id Init)
        #'(void)
        )
    (define-syntax (GENERATE stx)
        (with-syntax ([(#s(Var1 Id Init) ...) VARS])
            #'(begin
                (define Id Init)
                ...
            )
        )
    )
)
(module Usage racket
    (provide (all-defined-out))
    (require (submod ".." Macro))
    (DEFVAR x 0)
    (DEFVAR y 2)
    (DEFVAR z 44)
    (GENERATE)
    (define aa 100)
    (define bb 200)
)

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
