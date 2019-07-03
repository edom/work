(module stc-racket-base racket/base

    ;;  Prevent wrong macro expansion due to forgetting to import symbols like the ellipsis (...) in phase 1.

    (require (for-syntax racket/base))
    (provide (for-syntax (all-from-out racket/base)))

    (require
        racket/function
        racket/stream
        racket/string
    )

    (provide

        ;;  arithmetics and comparison

        *
        +
        -
        =
        <
        <=
        >
        >=
        eq?
        eqv?
        equal?

        ;;  conditional

        and
        case
        cond
        if
        not
        or
        unless
        when

        ;;  binding

        apply
        begin
        case-lambda
        curry
        define
        define-values
        error
        lambda
        let
        let*
        let-values
        letrec
        values
        parameterize
        procedure-arity
        procedure-arity?
        procedure?

        ;;  syntax and macros

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

        ;;  https://docs.racket-lang.org/reference/strings.html

        string
        string?
        symbol->string
        string->symbol
        bytes->string/utf-8
        substring
        string-copy
        string-append
        string->list
        list->string
        string=?
        string<?
        string<=?
        string>?
        string>=?
        string-ci=?
        string-ci<?
        string-ci<=?
        string-ci>?
        string-ci>=?
        string-upcase
        string-downcase
        string-join
        string-split
        string-trim
        string-replace
        non-empty-string?
        string-contains?
        string-prefix?
        string-suffix?

        ;;  data structures (product types)

        (rename-out
            (struct             define-struct)
        )
        struct-copy

        ;;  iteration and comprehension

        for
        in-stream

        ;;  regular expressions

        regexp-match-positions
        regexp-match
    )
)
