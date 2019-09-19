#lang scribble/lp2

@(require scribble/manual)

@title{STC-Rackbol user guide}

Here we present an example for you to judge whether you like STC-Rackbol or not.

@(table-of-contents)

@section{An STC-Rackbol program}

@code{#lang racket}

An STC-Rackbol program contains mostly definitions.

A program may also contains hints for the translator.

@chunk[|<an STC-Rackbol program>|
    <requires>
    |<define storages>|
    |<define tables>|
    |<define procedures>|
    |<other examples>|
]

@section{Require the STC-Rackbol module}

@chunk[<requires>
    (require "../stc-racket/stc-rackbol.rkt")
]

@section{Define storages}

This should mean what you think it means.

@chunk[|<define storages>|
    (DEFINE STORAGE pg_1
        TYPE postgresql
        HOST localhost
        PORT 5432
        USER test
        PASSWORD test
        CATALOG test
    )
]

@section{Define tables}

A table lives in a storage.

@chunk[|<define tables>|
    (DEFINE TABLE tab_1
        STORAGE pg_1
        SCHEMA company
        NAME employee
    )
]

@section{Define procedures}

A procedure should be either a command or a query.

A procedure named "Proc" translates to one HTML form in the URL path "/Proc" with both GET and POST method.
Each procedure input translates to a HTML input control.

@chunk[|<define procedures>|
    (DEFINE PROCEDURE test
        (INPUT x NAME "first number" TYPE Integer DEFAULT 0)
        (INPUT y TYPE Integer DEFAULT 1)
        (OUTPUT z TYPE Integer)
        (ACTION
            (set! z (+ x y))
        )
    )
]

@section{Define servers}

@section{Operate the system}

You can connect to a PostgreSQL storage from Racket shell with @racket[psql].
This requires the external program /usr/bin/psql.

@chunk[|<psql example>|
    (psql pg_1)
]

@section{Other examples}

@chunk[|<other examples>|
    (DEFINE VALUE x (+ 2 5))

    (pretty-print DEFINITIONS)

    (psql pg_1)

    (test)
    (test)
]
