(module stc-racket-base racket/base

    ;;  Prevent wrong macro expansion due to forgetting to import symbols like the ellipsis (...) in phase 1.

    (require (for-syntax racket/base))
    (provide (for-syntax (all-from-out racket/base)))

    (require
        racket/function
        racket/promise
        racket/stream
        racket/string
        racket/pretty
    )

    (provide

        ;;  arithmetics, logic, and comparison

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
        number?
        integer?
        boolean?

        and
        or
        not
        andmap
        ormap

        ;;  promise

        delay
        force

        ;;  conditional

        case
        cond
        else
        if
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
        set!

        ;;  https://docs.racket-lang.org/reference/strings.html

        string
        string?
        bytes->string/utf-8
        string->list
        list->string

        symbol?
        symbol->string
        string->symbol

        substring
        string-copy
        string-append
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

        define-struct
        (rename-out
            (struct             define-struct-2)
        )
        struct-copy

        ;;  iteration and comprehension

        for
        for/list
        in-list
        in-stream

        ;;  regular expressions

        regexp-match-positions
        regexp-match
    )

    (require "module.rkt")

    (require+provide racket/pretty
        pretty-print
    )
)
