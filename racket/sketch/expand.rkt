#lang stc-racket

(define-syntax-rule (printeval exp ...)
    (begin
        (printf "~v --> ~v~n" 'exp exp) ...
    )
)


(define (f x) (+ x 1))

#|
;;  https://docs.racket-lang.org/macro-debugger/index.html
;;  useful for debugging macro expansions
(require macro-debugger/stepper)
(expand/step #'(printeval (f 1) (f 1)))
|#

(printeval
    (f 1)
    (f 2)
    (f 3)
)


(require
    (only-in racket/base
        call-with-default-reading-parameterization
    )
)

(define (dump)
    (define (rec)
        (define stx (read-syntax))
        (unless (eof-object? stx)
            (printf "~v  ~~~  ~v~n" stx (syntax->datum stx))
            (rec)
        )
    )
    (call-with-default-reading-parameterization rec)
)

(with-input-from-file "load.rkt" (lambda ()
    (dump)
))
