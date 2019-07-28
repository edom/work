(module stc-racket-module racket/base

    ;;  --------------------    Racket modules.

    (require "syntax.rkt")

    (provide require+provide)

    (define-syntax-rule (require+provide Module Import ...)
        (begin
            (require (only-in Module Import ...))
            (imports->provide Import ...)
        )
    )

        ;;  Helper for "require+provide".

        (define-syntax-rules imports->provide ()
            ((_ (Original Renamed))     (provide Renamed))
            ((_ Original)               (provide Original))
            ((_ Import ...)             (begin (imports->provide Import) ...))
        )

    (provide

        (rename-out
            (module             define-module)
            (module*            define-module*)
            (module+            define-module+)
        )
        provide
        require

        ;;  require-forms and provide-forms
        ;;  https://docs.racket-lang.org/reference/require.html

        only-in
        except-in
        prefix-in
        rename-in
        combine-in
        relative-in
        only-meta-in
        for-syntax
        for-template
        for-label
        for-meta
        submod
        lib
        file
        planet

        all-defined-out
        all-from-out
        rename-out
        except-out
        prefix-out
        struct-out
        combine-out
        protect-out

        ;;  namespaces

        current-namespace
        make-base-namespace
    )
)
