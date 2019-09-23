#lang scribble/lp2

@(require scribble/manual)

@title{Rackbol user guide}

Here we present an example for you to judge whether you like Rackbol or not.

@(table-of-contents)

@section{An example Rackbol program}

@racketmod[racket]

A Rackbol program contains mostly definitions.

A program may also contains hints for the translator.

@chunk[|<a Rackbol program>|
    |<define storages>|
    |<define tables>|
    |<define procedures>|
    |<other examples>|
]

Make sure that you set the PLTCOLLECTS environment variable to include
the @emph{parent} directory of the stc-racket directory.

@section{Define storages}

This should mean what you think it means.

@chunk[|<define storages>|
    #f
]

@section{Define tables}

A table lives in a storage.

@chunk[|<define tables>|
    #f
]

@section{Define procedures}

A procedure should be either a command or a query.

A procedure named "Proc" translates to one HTML form in the URL path "/Proc" with both GET and POST method.
Each procedure input translates to a HTML input control.

@chunk[|<define procedures>|
    #f
]

@section{Define servers}

@section{Operate the system}

You can connect to a PostgreSQL storage from Racket shell with @racket[psql].
This requires the external program /usr/bin/psql.

@chunk[|<psql example>|
    #f
]

@section{Other examples}

@chunk[|<other examples>|
    #f
]
