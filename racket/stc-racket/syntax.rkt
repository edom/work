;;  syntax and macros

(module stc-racket-syntax racket/base

    (provide

        begin-for-syntax
        define-syntax
        define-syntax-rule
        syntax-case
        syntax-rules

        datum->syntax
        syntax->datum

        quasiquote
        quote
        syntax

        syntax?
        identifier?

        syntax-e

        identifier-binding

        ;;  expansion

        expand

        ;;  syntax object properties

        syntax-source
    )
)
