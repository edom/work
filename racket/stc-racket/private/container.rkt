(module stc-racket-container racket/base

    (require
        racket/stream
        racket/vector
    )

    (require
        (rename-in racket/base
            (append             list-append)
            (filter             list-filter)
            (map                list-map)
            (member             list-member)
            (null?              empty-list?)
        )
        (rename-in racket/list
            (empty              empty-list)
        )
    )

    (provide

        ;;  containers, data structures

        empty-list
        empty-list?
        list
        list?
        list-map
        list-filter
        list-append
        list-member
        list-prefix?

        cons
        cons?
        car
        cdr

        hash
        hasheq
        hasheqv
        make-hash
        hash?
        hash-ref
        hash-set!
        hash-remove!
        hash->list

        vector
        vector?
        vector-map
        vector-ref
        vector->list

        ;; customizations

        container-map
        container-ref
        container-set!
    )

    ;;  Should use generics or multimethods?

    (define (container-map f x)
        (cond
            ((cons? x)          (list-map f x))
            ((empty-list? x)    (list-map f x))
            ((vector? x)        (vector-map f x))
            ((stream? x)        (stream-map f x))
            (else               (error "container-map" f x))
        )
    )

    (define (container-ref f x)
        (cond
            ((vector? x)        (vector-ref f x))
            ((hash? x)          (hash-ref f x))
            (else               (error "container-ref" f x))
        )
    )

    (define (container-set! c i v)
        (cond
            ((vector? c)        (vector-set! c i v))
            ((hash? c)          (hash-set! c i v))
            (else               (error "container-set!" c i v))
        )
    )
)
