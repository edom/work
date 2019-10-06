#lang rackbol

;;  To run this example, enter this into Bash:
;;
;;      PLTCOLLECTS=$PWD: racket -t rackbol/example.rkt -m
;;
;;  The PLTCOLLECTS environment variable must include
;;  the @emph{parent} directory of the stc-racket directory.

;;  --------------------    Introduction
;;
;;  A Rackbol program contains mostly definitions.
;;
;;  A program may also contains hints for the translator.

;;  --------------------    Define storages.
;;
;;  This should mean what you think it means.

(DEFINE STORAGE pg_1
    TYPE postgresql
    HOST localhost
    PORT 5432
    CATALOG test
    USER test
    PASSWORD test
)

(DEFINE TABLE tab_1
    STORAGE pg_1
    SCHEMA company
    NAME employee
)

(DEFINE PROCEDURE p_test
    (INPUT x NAME "first number" TYPE Integer DEFAULT 0)
    (INPUT y TYPE Integer DEFAULT 1)
    (OUTPUT z TYPE Integer)
    (ACTION
        (set! z (+ x y))
    )
)

(define-constant x (+ 2 5))

(DEFINE SERVER s_1
    PROTOCOL HTTP
    ADDRESS "127.0.0.1"
    PORT 8080
)

#|
(psql pg_1)
(CALL p_test)
(CALL p_test)
|#
