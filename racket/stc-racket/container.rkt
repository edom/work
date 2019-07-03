(module stc-racket-container racket/base

    (require
        racket/list
        racket/stream
        racket/vector
    )

    (require
        (rename-in racket/base
            (append             list-append)
            (map                list-map)
        )
    )

    (provide

        ;;  containers, data structures

        list
        cons?
        list?
        list-map
        list-append

        hash
        hasheq
        hasheqv
        make-hash
        hash?
        hash-ref
        hash-set!
        hash-remove!

        vector
        vector?
        vector-map
        vector-ref

        ;; customizations

        container-map
        container-ref
        container-set!
    )

    ;;  Should use generics or multimethods?

    (define (container-map f x)
        (cond
            ((cons? x)      (list-map f x))
            ((null? x)      (list-map f x))
            ((vector? x)    (vector-map f x))
            ((stream? x)    (stream-map f x))
            (else           (error "container-map" f x))
        )
    )

    (define (container-ref f x)
        (cond
            ((vector? x)    (vector-ref f x))
            ((hash? x)      (hash-ref f x))
            (else           (error "container-ref" f x))
        )
    )

    (define (container-set! c i v)
        (cond
            ((vector? c)    (vector-set! c i v))
            ((hash? c)      (hash-set! c i v))
            (else           (error "container-set!" c i v))
        )
    )
)
