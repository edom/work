#lang s-exp "stc-racket.rkt"

(define-syntax-rule (printeval exp ...)
    (begin
        (printf "~v --> ~v~n" 'exp exp) ...
    )
)

(define (f x) (+ x 1))
(define v #(1 2 3))
(define h #hash((x . 1) (y . 2)))

(define-struct Person (name age))

(printf "~v~n" (Person "john" 30))



#|

;;  https://docs.racket-lang.org/macro-debugger/index.html
;;  useful for debugging macro expansions

(require macro-debugger/stepper)

(expand/step #'(printeval (f 1) (f 1)))

|#



(printeval
    (f 1)
    (v 2)
    (h 'x)
    (h 'y)
    (#hash((x . 11) (y . 22)) 'x)
    (#hash((x . 11) (y . 22)) 'y)
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
