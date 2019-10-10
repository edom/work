#lang s-exp "base.rkt"

;;  --------------------    Operation console.

(require "readline.rkt")

(provide
    (for-syntax
        generate-operation-console
    )
    help
    admin
    run
    quit
)

;;  --------------------    Shared runtime for operation console.

(define (help) (displayln #<<END_OF_HELP
(quit)

    Quit the operation console.

(admin Id)

    Open the admin console of a resource.
    If Id refers to a postgresql storage,
    this starts a psql process that connects to that server.

(help)

    Show this help.
END_OF_HELP
))

;;  Racket does not check undefined identifiers in templates
;;  until they are instantiated.
;;  Thus, for example, if "GET" were undefined, then
;;  the "_admin" procedure would cause a compile-time error
;;  but the "admin" form would cause an expand-time error,
;;  which is a run-time error if the form is expanded at run-time.

(define-syntax-rule (admin Id) (_admin 'Id Id))
(define-syntax-rule (run Id) (_run 'Id Id))

(define (_admin id object)
    (with-original-stdin
        ([GET open-admin-console OF object OR
            (printf "Don't know how to administer ~a." id)])
    ))

(define (_run id object)
    (with-original-stdin
        ([GET run OF object OR
            (printf "Don't know how to run ~a." id)])
    ))

(define (quit)
    (exit))

(define (greet source)
    (printf "Welcome to the Rackbol operation console for ~a.~n" source)
    (displayln #<<END_OF_GREETINGS
For help, type (help) and press Enter.
To quit, press Ctrl+D.
END_OF_GREETINGS
    ))

(define (prettier-print thing)
    (unless (void? thing)
        (pretty-print thing)
    ))

;;  --------------------    Generate.

(define-for-syntax (generate-operation-console stx run-id)
    (define _src (syntax-source stx))
    (define source (if _src (path->string _src) "<unknown source>"))
    #`(begin

        (require
            (relative-in com-spacetimecat/rackbol/lang
                "operation.rkt"
                "readline.rkt"
            )
            racket/pretty
        )

        ;;  It is possible that our reserved identifiers clash with the user's identifiers.

        (provide

            ;;  Predefined actions.

            help
            admin
            run
            quit

            ;;  For read-eval-print-loop.

            #%app
            #%top-interaction
            #%top
            #%datum

        )

        (define (#,run-id)
            (check-stdin)
            (greet #,source)
            (parameterize ([current-print prettier-print])
                (read-eval-print-loop)
            )
        )

        ;;  See readline.rkt for details.
        (define (check-stdin)
            (unless (terminal-port? (get-original-stdin))
                (displayln "Warning: Standard input is not a terminal. This may confuse subprocesses."))
        )

    ))
