#lang rackbol/web

;;  To run this example, enter this into Bash:
;;
;;      #   If you are running this from the parent directory:
;;      ./racket-env.sh racket -t rackbol/example.rkt -m
;;
;;      #   If you are running this from the directory containing this file:
;;      ../racket-env.sh racket -t example.rkt -m

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
(CALL p_test)
(CALL p_test)
|#

;;  --------------------    Interaction.

(GENERATE OPERATION CONSOLE
    TO RUN CALL run-operation-console
)

(define (main)
    (run-operation-console)
)

(provide main)
