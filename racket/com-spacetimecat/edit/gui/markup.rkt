#lang s-exp "lang.rkt"

(provide
    markup?
    empty
    string
    bold
    hflow
    vflow
)

(define (markup? x) (match x
    [`(empty)       #t]
    [`(string ,s)   (string? s)]
    [`(bold ,m)     (markup? m)]
    [`(hflow ,ms)   (list-each-satisfies? markup? ms)]
    [`(vflow ,ms)   (list-each-satisfies? markup? ms)]
    [_              #f]
))

(define (empty) (list 'empty))

(define/contract (string x)
    (-> string? markup?)
    (list 'string x)
)

(define/contract (bold prog)
    (-> markup? markup?)
    (list 'bold prog)
)

(define/contract (hflow progs)
    (-> (listof markup?) markup?)
    (list 'hflow progs)
)

(define/contract (vflow progs)
    (-> (listof markup?) markup?)
    (list 'vflow progs)
)
