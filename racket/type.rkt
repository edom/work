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
    (define-type (Any))

    (define-type (Function a b))

    (define %subtype (%rel (a)
        ((Integer Number))
        ((a a))
        ((a Any))
    ))

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
            ((app function-type type) #:when type   type)
            (_                                      (Any))
        )
    )

    ;;  To-do:

    ;;  Type-checking is abstract interpretation in which the semantics function is the type-of function.

)
