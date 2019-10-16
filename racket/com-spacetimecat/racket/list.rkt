#lang s-exp "lang.rkt"

(require+provide
    (rename-in racket/base
        (cons make-pair)
        (null? list-empty?)
        (append list-append)
        (length list-length)
        (filter list-filter)
        (map list-map)
        (sort list-sort)
        (foldl list-foldl)
        (foldr list-foldr)
    )
    (rename-in racket/list
        (empty list-empty)
        (filter-map list-filter-map)
        (partition list-partition)
        (remove-duplicates list-remove-duplicates)
        (take list-take)
        (argmax list-argmax)
        (append-map list-append-map)
    )
)

(provide

    list-concatenate
    list-concat-map
    list-take-at-most

    list-compare

)

(define (list-concatenate lists) (apply list-append lists))
(define (list-concat-map f list) (list-concatenate (map f list)))

(define/contract (list-take-at-most count #:from list)
    (-> exact-nonnegative-integer?
        #:from (listof any/c)
        (listof any/c))
    (define (loop count list)
        (if (or (= count 0) (list-empty? list))
            list-empty
            (cons (car list) (loop (- count 1) (cdr list)))
        )
    )
    (loop count list)
)

;;  Lexicographical comparison.
;;
;;  To compare a list of numbers,
;;  use #:with - (subtraction) as compare-element.

(define/contract (list-compare a b #:with compare-element)
    (-> (listof any/c)
        (listof any/c)
        #:with (-> any/c any/c real?)
        real?
    )
    (define (loop a b)
        (if (or (list-empty? a) (list-empty? b))
            (- (list-length a) (list-length b))
            (let ([d (compare-element (car a) (car b))])
                (if (zero? d)
                    (loop (cdr a) (cdr b))
                    d
                )
            )
        )
    )
    (loop a b)
)
