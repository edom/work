#lang rackbol

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

(DEFINE PROCEDURE test
    (INPUT x NAME "first number" TYPE Integer DEFAULT 0)
    (INPUT y TYPE Integer DEFAULT 1)
    (OUTPUT z TYPE Integer)
    (ACTION
        (set! z (+ x y))
    )
)

(DEFINE VALUE x (+ 2 5))

(pretty-print DEFINITIONS)

(psql pg_1)

(test)
(test)
