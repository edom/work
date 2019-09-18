#lang racket

;;  --------------------    A business-oriented language, or a CRUD-Web-application-oriented language.

(require (only-in racket/pretty pretty-print))
(require (for-syntax syntax/parse))
(require (for-syntax syntax/strip-context))
(require "racket-extra.rkt")

(provide (rename-out (module-begin #%module-begin)))
(provide (rename-out (my_define define)))
(provide (rename-out (my_top #%top)))
(provide #%app)
(provide #%datum)
(provide quote)
(provide unquote)
(provide +)
(provide DEFINITIONS)
(provide pretty-print)

;;  --------------------    Operations.

(provide psql)

    (provide alist-ref)

    (define (alist-ref alist key)
        (define pair (assoc key alist))
        (if pair    (cdr pair)
                    (error 'alist-ref "undefined key: ~a" key) ))

    (define-syntax-rule (system**/exit-code Path Arg ...)
        (system*/exit-code
            Path
            (string->bytes/utf-8 (->string Arg))
            ...))

    ;;  Start psql shell.
    ;;  The "psql" binary must be installed at "/usr/bin/psql".

    (define (psql storage)
        (define psql_bin "/usr/bin/psql")
        (deconstruct storage WITH alist-ref TO (host port catalog user password))
        (with-environment ((PGPASSWORD password))
            (define exit_code (system**/exit-code psql_bin "-h" host "-p" port catalog user))
            (unless (= exit_code 0)
                (error 'psql "subprocess ~a exited with code ~a" psql_bin exit_code))
        ))

;;  https://stackoverflow.com/questions/38130826/can-i-instantiate-a-module-multiple-times-in-one-racket-program

    (define DEFINITIONS (make-hash))

    (define-syntax-rule (module-begin Exp ...)
        (#%module-begin Exp ...))

    (define-syntax-rule (my_top . Id)
        (hash-ref DEFINITIONS 'Id (lambda () (error "undefined variable:" 'Id))))

;;  The "define" form.

    (begin-for-syntax
        (define-syntax-class type%
            #:datum-literals (storage table)
            (pattern storage)
            (pattern table)
        )
        (define-splicing-syntax-class procedure_input%
            #:datum-literals (input type default)
            (pattern (input Id
                (~optional (~seq name Name) #:defaults ((Name #''Id)))
                type Type
                (~optional (~seq default Default) #:defaults ((Default #f)))
            ))
        )
        (define-splicing-syntax-class procedure_output%
            #:datum-literals (output type)
            (pattern (output Id
                (~optional (~seq name Name) #:defaults ((Name #''Id)))
                type Type
            ))
        )
    )

    (define (format-date/iso x)
        (define offset (date-time-zone-offset x))
        (define-values (offset-hour offset-m) (quotient/remainder offset 3600))
        (define offset-minute (quotient offset-m 60))
        (format "~a-~a-~aT~a:~a:~a~a~a:~a"
            ;;  TODO: left-pad with zero
            (date-year x)
            (date-month x)
            (date-day x)
            (date-hour x)
            (date-minute x)
            (date-second x)
            (if (nonnegative-integer? offset) "+" "")
            offset-hour
            offset-minute
        )
    )

    (require racket/date)
    (begin-for-syntax
        (define LOG_ENABLED #t)
    )
    (provide set!)
    (provide log)
    (define-syntax (log stx)
        (if LOG_ENABLED
            (syntax-case stx ()
                [(_ Message)
                    #'(let (
                            (time (current-date))
                            (level "INFO")
                        )
                        (printf "~a ~a ~s~n" (format-date/iso time) level Message))
                ]
            )
            #'(void)))

    (define (read_input name)
        (printf "Input ~a: " name)
        (read))

    (define-syntax (make_alist stx)
        (syntax-parse stx
            ((_ (~seq Key Val) ...)
                #'(list (cons `Key Val) ...))))

    (define-syntax (my_define stx)
        (syntax-parse stx
            #:datum-literals (procedure action value)
            ((_ Type:type% Name (~seq Key Val) ...)
                #'(hash-set! DEFINITIONS 'Name '((Key . Val) ...))
            )
            ((_ value Name Value)
                #'(hash-set! DEFINITIONS 'Name Value)
            )
            ((_ procedure Name
                    (~alt
                        Input:procedure_input%
                        Output:procedure_output%
                        (~once (action Action ...))
                    )
                    ...
                )
                #'(let ((Code (lambda ()
                            (define version 0)
                            (define execution_id (random 1048576))
                            (define t0 (current-milliseconds))
                            (log (list version execution_id 'Name 'start))
                            (define Input.Id (read_input Input.Name)) ...
                            (define Output.Id #f) ...
                            Action ...
                            (printf "~a = ~v~n" Output.Name Output.Id) ...
                            (define t1 (current-milliseconds))
                            (log (list version execution_id 'Name 'stop (- t1 t0)))
                    )))
                    (hash-set! DEFINITIONS 'Name Code)
                    #|
                    ;;  Should we define struct Procedure with a prop:procedure property?
                    (hash-set! DEFINITIONS 'Name `(
                        (type . procedure)
                        (name . Name)
                        (inputs . ((Input.Id Input.Type) ...))
                        (outputs . (Output.Id ...))
                        (action . #'(Action ...))
                        (code . ,Code)
                    ))
                    |#
                )
            )
        ))
