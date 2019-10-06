#lang s-exp "base.rkt"

(provide
    _DEFINE_STORAGE
)

;;  --------------------    syntax/parse enhancements.

(begin-for-syntax
    (require
        (for-syntax
            racket/base
            syntax/parse
        )
        racket/base
    )
    (provide
        ~permute
        ~permute-seq
    )
    ;;  This is an EH-pattern (ellipsis-head pattern).
    ;;  Do not forget the trailing ellipsis after the last closing parenthesis.
    ;;
    ;;  For example:
    ;;
    ;;      (~permute A B C) ...
    ;;
    ;;  translates to:
    ;;
    ;;      (~alt
    ;;          (~once A)
    ;;          (~once B)
    ;;          (~once C)
    ;;      ) ...
    ;;
    ;;  For example:
    ;;
    ;;      (~permute-seq
    ;;          (A0 A1 A2)
    ;;          (B0 B1 B2)
    ;;          (C0 C1 C2)
    ;;      ) ...
    ;;
    ;;  translates to:
    ;;
    ;;      (~alt
    ;;          (~once (~seq A0 A1 A2))
    ;;          (~once (~seq B0 B1 B2))
    ;;          (~once (~seq C0 C1 C2))
    ;;      ) ...

    (define-syntax ~permute
        (pattern-expander (lambda (stx)
            (syntax-case stx ()
                [(_ Thing ...)
                    #'(~alt (~once Thing) ...)
                ]))))

    (define-syntax ~permute-seq
        (pattern-expander (lambda (stx)
            (syntax-case stx ()
                [(_ (Thing ...) ...)
                    #'(~permute (~seq Thing ...) ...)
                ]))))
)

;;  --------------------    DEFINE STORAGE form.

(define-syntax-parser _DEFINE_STORAGE
    #:datum-literals (CATALOG HOST PASSWORD PORT postgresql TYPE USER)
    [   (_ Id
            TYPE postgresql
            (~permute-seq
                [HOST Host]
                [PORT Port]
                [CATALOG Catalog]
                [USER User]
                [PASSWORD Password]
            ) ...
        )
        #'(begin
            (_DEFINE_OBJECT Id TYPE Storage WITH
                [type postgresql] [host Host] [port Port]
                [catalog Catalog] [user User] [password Password]
                [open-admin-console ,(λ => psql Id)]
            )
            (provide Id)
        )
    ])

;;  --------------------    Operations.

(define psql_bin "/usr/bin/psql")

;;  Start psql shell.
;;  The "psql" binary must be installed at "/usr/bin/psql".

(define (psql storage)
    (DECONSTRUCT storage WITH get-object-property TO (host port catalog user password))
    (with-environment ((PGPASSWORD password))
        (define exit_code (system**/exit-code psql_bin "-h" host "-p" port catalog user))
        (unless (= exit_code 0)
            (error 'psql "subprocess ~a exited with code ~a" psql_bin exit_code))
    ))
