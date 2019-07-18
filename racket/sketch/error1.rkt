;; Reexports. Nothing suspicious.

(module error1 racket/base
    (require
        (for-syntax racket/base)
    )
    (provide
        #%app
        #%datum
        #%module-begin
        +
        quote
        begin
        printf
        define-syntax
        define-syntax-rule

        ; This is rather obscure: Omitting the following line causes subtle errors in error2.rkt.
        ; (for-syntax ...)

        (for-syntax (all-from-out racket/base))
    )
)
