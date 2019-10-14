#lang s-exp "lang.rkt"

(provide
    (all-defined-out)
)

(define markup/c
    (recursive-contract
        (or/c   (list/c 'empty)
                (list/c 'string string?)
                (list/c 'bold markup/c)
                (list/c 'hflow (listof markup/c))
        )
    )
)

(define (empty) (list 'empty))

(define/contract (string x)
    (-> string? markup/c)
    (list 'string x)
)

(define/contract (bold prog)
    (-> markup/c markup/c)
    (list 'bold prog)
)

(define/contract (hflow progs)
    (-> (listof markup/c) markup/c)
    (list 'hflow progs)
)
