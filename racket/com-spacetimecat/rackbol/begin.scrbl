#lang stc-racket/scribble-manual

@title{Beginning Rackbol}

@section{Rackbol is built on top of Racket}

Here are the reexports of the @racketmodname[rackbol] module.

From @racketmodname[racket/base], we reexport almost everything,
but we shadow @racket[λ].

From @racketmodname[racket/pretty], we reexport @racket[pretty-print].

From @racketmodname[racket/include], we reexport @racket[include].

You can write @emph{fewer parentheses} with @racket[λ], @racket[LET],
and its variants @racket[LET*], @racket[LETREC], and @racket[LETREC*].
But we don't aim to eliminate parentheses completely.
This is just a minor usability enhancement.

@section{Define some storages and tables}

Most values are quasiquoted by default because, in our experience,
most values fed to @racket[DEFINE] are constant literals.

Define some storages and tables.

@racketblock[
    (DEFINE STORAGE pg_1
        TYPE postgresql
        HOST localhost
        PORT 5432
        CATALOG test
        USER test
        PASSWORD test
    )
]

A table lives in a storage.

@section{Define some procedures}

What to do with data.

A procedure should be either a command or a query.

@section{Operate your system}

After you develop a software system, you operate it.

Open your system's operation console by running this, where FILE is the path to your Rackbol file:

@verbatim{
racket -t FILE -m
}

Explore the console by entering "(help)" (without quotes).

Security Problem of the "admin" command:
Because we pass the password to "psql" with the PGPASSWORD environment variable,
the password leaks in the file "/proc/<pid>/environ", which is readable by the user that psql is running as,
which is the user that your Racket interpreter runs as, which is your user.
Thus, practically all other running programs can read the password.
However, we assume that you control your machine.
