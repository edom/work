#lang s-exp "base.rkt"

;;  --------------------    Operation console.

(require "readline.rkt")

(provide
    help
    admin
    run
    quit
)

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
