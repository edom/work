#lang rackbol

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

(define (show-tables)
    (FOR EACH ROW IN TABLE pg_1 information_schema tables
        AS t (table_catalog table_schema table_name)
        DO  (printf "~a ~a ~a~n" t.table_catalog t.table_schema t.table_name)
    )
)

(define (show-columns)
    (FOR EACH ROW IN TABLE pg_1 information_schema columns
        AS c (table_catalog table_schema table_name column_name data_type)
        DO  (printf "~a ~a ~a ~a ~a~n" c.table_catalog c.table_schema c.table_name c.column_name c.data_type)
    )
)

(provide
    show-tables
    show-columns
)

;;  --------------------    Logic programming.

(require rackbol/private/logic)

(define %father_child
    (%rel ()
        [('f1 'c1)]
        [('f2 'c2)]
    ))

(define %mother_child
    (%rel ()
        [('m1 'c1)]
        [('m2 'c2)]
    ))

(define (logic)
    (%for-each F M C #:satisfying
        (%father_child F C)
        (%mother_child M C)
    #:do
        (printf "~a ~a ~a~n" F M C)
    )

    (%FOR-EACH F M C SATISFYING
        (%father_child F C)
        (%mother_child M C)
    DO
        (printf "~a ~a ~a~n" F M C)
    )
)

(provide logic)
