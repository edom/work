(module type "stc-racket.rkt"

    (provide (all-defined-out))

    ;;  A type here is an expression that denotes a set of values.

    ;;  A lambda denotes a type function
    ;;  a.k.a. a family of types
    ;;  a.k.a. a parametrically-polymorphic type
    ;;  a.k.a. a family of computable sets.

    (define-syntax-rule (define-type (name param ...))
        (define-struct name (param ...) #:prefab)
    )

    ;; empty set, zero type, null set, void type, false, absurd, contradiction

    (define-type (Empty))
    (define-type (Type-Error message))

    (define-type (Number))
    (define-type (Real))
    (define-type (Integer))
    (define-type (Cons))
    (define-type (Boolean))
    (define-type (Any))

    (define-type (Function a b))

    ;;  If f : T -> U and x : A, then:
    ;;      Iff T intersect A is not empty, then (f x) : U.
    ;;      Otherwise (f x) : (Type-Error message).

    (define-struct-2 Typed (syntax type) #:prefab)

    ;;  This is for pretty-printing only.
    ;;  This has no particular semantics.

    (define (Typed->datum object)
        (match object
            ((Typed syntax type)
                (list
                    (syntax->datum syntax)
                    ':
                    type
                )
            )
        )
    )

    (define known-functions (hash
        + (Function (Number) (Number))
        * (Function (Number) (Number))
        - (Function (Number) (Number))
    ))

    (define (function-type fun)
        (hash-ref known-functions fun #f)
    )

    ;;  The type of the object at first sight lonely by itself without any context.

    (define (apparent-type-of object)
        (match object
            ((cons _ _)                             (Cons))
            ((? integer? object)                    (Integer))
            ((? boolean? object)                    (Boolean))
            ((app function-type type) #:when type   type)
            (_                                      (Any))
        )
    )

    ;;  Decorate a syntax object with type information.

    (define-module decoration racket/base
        (require
            '#%kernel
            (rename-in '#%kernel
                (lambda #%lambda)
            )
        )
        (provide
            #%module-begin
            #%require
            #%app
            module
            #%lambda
            :
        )
        (define (: expr type) expr)
    )

    (require
        'decoration
        (for-template 'decoration)
    )

    (define (decorate stx)

        (printf "decorate ~v~n" stx)

        (define my-lexical-scope #'())

        (define (ERROR message object)
            (raise-syntax-error 'decorate message object)
        )

        (define (datsyn object)
            (datum->syntax my-lexical-scope object)
        )

        (syntax-case stx
            (
                module
                #%module-begin
                #%require
                #%app
                #%lambda
                define-values
                quote
                :
            )
            ((module Name Base Body)
                #`(module Name Base #,(decorate #'Body))
            )
            ((#%module-begin . Body)
                #`(#%module-begin . #,(decorate-list #'Body))
            )
            ((#%require . A)   #'(#%require . A))
            ((#%app F . A)
                ;; TODO
                #`(#%app F . #,(decorate-list #'A))
            )
            ((define-values Name Expr)
                ;; TODO
                #`(define-values Name #,(decorate #'Expr))
            )
            ((quote A)
                #`(:
                    (quote A)
                    #,(datsyn (apparent-type-of (syntax->datum #'A)))
                )
            )
            ((#%lambda Param . Body)
                ;; TODO
                #`(#%lambda Param . Body)
            )
            (Var (identifier? #'Var)
                ;; TODO
                #`(:
                    Var
                    #,(Any)
                )
            )
            (_
                (ERROR "not implemented for" stx)
            )
        )
    )
    (define (decorate-list stx)
        (printf "decorate-list ~v~n" stx)
        (define (ERROR message object)
            (raise-syntax-error 'decorate-list message object)
        )
        (syntax-case stx ()
            (() #'())
            ((A . B) #`(#,(decorate #'A) . #,(decorate-list #'B)))
        )
    )

    ;;  To-do:

    ;;  Type-checking is abstract interpretation in which the semantics function is the type-of function.

)
