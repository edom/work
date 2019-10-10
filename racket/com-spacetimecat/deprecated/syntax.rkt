(module stc-racket-syntax racket/base

    ;;  TODO: Turn this into a guide/tutorial.

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
        unquote
        syntax
        quasisyntax
        unsyntax

        with-syntax

        let-syntax

        syntax?
        identifier?

        syntax-e

        identifier-binding
        free-identifier=?
        bound-identifier=?

        ;;  expansion

        expand

        ;;  syntax object properties

        syntax-source

        syntax-local-value

        syntax-property
        syntax-property-remove

        ;;  error reporting

        raise-syntax-error

        ;;  TODO: syntax/parse
    )
)
