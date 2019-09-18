#lang s-exp "dsl.rkt"

(define storage pg_1
    type postgresql
    host localhost
    port 5432
    user test
    password test
    catalog test
)

(define table tab_1
    storage pg_1
    schema company
    name employee
)

(define procedure test
    (input x name "first number" type Integer default 0)
    (input y type Integer default 1)
    (output z type Integer)
    (action
        (set! z (+ x y))
    )
)

(define value x (+ 2 5))

(psql pg_1)

(pretty-print DEFINITIONS)

(test)
(test)

