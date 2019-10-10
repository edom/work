#lang s-exp "base.rkt"

;;  --------------------    DEFINE STORAGE form.

(define-syntax-parser _DEFINE_STORAGE
    #:datum-literals (CATALOG HOST PASSWORD PORT postgresql TYPE USER)
    [   (_ Id:id
            Stor:Storage$
            ...
        )
        #'(begin
            (_DEFINE_OBJECT Id TYPE Storage WITH
                [type postgresql] [host Stor.host] [port Stor.port]
                [catalog Stor.catalog] [user Stor.user] [password Stor.password]
                [open-admin-console ,(λ => psql Id)]
            )
            (provide Stor.id)
        )
    ])

;;  Automatic per-thread magic that is supposed to "just work",
;;  provided that no threads are killed.
;;
;;  Question: Does a custodian still work if its thread is killed instead of
;;  if its thread finishes normally?

(define (make-virtual-connection storage)
    (define host (->string (GET host OF storage)))
    (define port (GET port OF storage))
    (define catalog (->string (GET catalog OF storage)))
    (define user (->string (GET user OF storage)))
    (define password (->string (GET password OF storage)))
    (virtual-connection
        (connection-pool
            (lambda ()
                (postgresql-connect
                    #:user user
                    #:database catalog
                    #:password password
                    #:server host
                )
            )
            #:max-connections 4
            #:max-idle-connections 4
        )
    )
)

(require (for-syntax "object.rkt"))

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
