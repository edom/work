(module stc-racket racket/base



;;;;    --------------------    imports



    (module imports racket/base
        (require
            (for-syntax racket/base)
            (only-in racket/function
                curry
            )
            (only-in racket/list
                cons?
            )
            (only-in racket/vector
                vector-map
            )
            racklog
        )
        (provide
            (for-syntax
                (all-from-out racket/base)
            )
            (all-from-out racket/function)
            (all-from-out racket/list)
            (all-from-out racket/vector)
            (all-from-out racklog)

            ;; racket/base

            (rename-out
                (map                list-map)
            )
            +
            begin
            call-with-input-file
            case
            case-lambda
            cond
            datum->syntax
            define
            define-syntax
            define-syntax-rule
            display
            eof-object?
            error
            hash-ref
            hash?
            if
            lambda
            list
            newline
            only-in
            parameterize
            printf
            procedure-arity
            procedure-arity?
            procedure?
            provide
            quote
            read
            read-syntax
            require
            syntax
            syntax->datum
            unless
            vector
            vector-ref
            vector?
            when
            with-input-from-file

            ;; racklog

            (rename-out
                (%assert!           %asserta!)
                (%assert-after!     %assertz!)
                (%fail              %false)
            )
            %/=
            %/==
            %<
            %<=
            %=
            %=/=
            %=:=
            %==
            %>
            %>=
            %and
            %append
            %bag-of
            %empty-rel
            %if-then-else
            %is
            %member
            %more
            %nonvar
            %not
            %or
            %rel
            %set-of
            %true
            %var
            %which
        )
    )

    (require 'imports)



;;;;    --------------------    exports



    (provide

        (all-from-out 'imports)

        (rename-out
            (app #%app)
        )

        #%datum
        #%module-begin
        #%top

        ;; customizations

        container-map
        container-ref
        container-set!

    )



;;;;    --------------------    customizations


    ;; Problem: This explodes the syntax tree and may slow down the interpreter.

    (define-syntax-rule (app fun arg ...)
        (cond
            ((procedure? fun)
                (fun arg ...)
            )
            ((hash? fun)
                (hash-ref fun arg ...)
            )
            ((vector? fun)
                (vector-ref fun arg ...)
            )
            (else
                (error "cannot apply" fun arg ...)
            )
        )
    )

    ;; Should use generics or multimethods?

    (define (container-map f x)
        (cond
            ((cons? x)      (list-map f x))
            ((null? x)      (list-map f x))
            ((vector? x)    (vector-map f x))
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
