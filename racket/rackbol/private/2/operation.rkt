#lang s-exp "_in.rkt"

;;  --------------------    Operations.

(provide (all-defined-out))

(define psql_bin "/usr/bin/psql")

;;  Start psql shell.
;;  The "psql" binary must be installed at "/usr/bin/psql".

(define (psql storage)
    (deconstruct storage WITH get_object_property TO (HOST PORT CATALOG USER PASSWORD))
    (with-environment ((PGPASSWORD PASSWORD))
        (define exit_code (system**/exit-code psql_bin "-h" HOST "-p" PORT CATALOG USER))
        (unless (= exit_code 0)
            (error 'psql "subprocess ~a exited with code ~a" psql_bin exit_code))
    ))
