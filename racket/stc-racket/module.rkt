;;  Racket modules.

(module stc-racket-module racket/base

    (provide

        (rename-out
            (module             define-module)
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
    )
)
