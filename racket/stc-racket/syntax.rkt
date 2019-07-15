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

        ;;  error reporting

        raise-syntax-error

        ;;  TODO: syntax/parse
    )
)