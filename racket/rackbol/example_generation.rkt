#lang racket

(begin-for-syntax
    (define TABLE (make-hash))
    (hash-set! TABLE 'alice 1)
    (hash-set! TABLE 'bob 2)
    (hash-set! TABLE 'charlie 3)
)

(define-syntax (GENERATE stx)
    (with-syntax (
            [(Message ...)
                (for/list ([(k v) (in-hash TABLE)])
                    (format "~a ~a" k v)
                )
            ]
        )
        #'(begin
            (begin
                (display Message)
                (newline)
            ) ...
        )
    )
)

;;  Expand this with DrRacket's Macro Stepper.

(GENERATE)
