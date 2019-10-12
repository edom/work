#lang stc-racket/scribble-manual

@title{The STC-Racket language}

@defmodule[stc-racket #:lang]

@section{The purpose of STC-Racket}

If you are not me, you will not find any strong reason to use STC-Racket.

The purpose of STC-Racket is
to gather the libraries that I have created, vetted, curated, or adopted.
Thus it is similar in spirit to Awesome Racket@fnurl{https://github.com/avelino/awesome-racket},
but more stringent, because I have used the libraries myself.


@section{Renaming some Racket exports for consistency}

The STC-Racket language renames some Racket exports for consistency.

@table[
    #:header (list "Racket" "STC-Racket")
    #:caption "Some renamings from Racket to STC-Racket"
    #:rows (list
        (list (racket module) (racket define-module))
        (list (racket module*) (racket define-module*))
        (list (racket module+) (racket define-module+))
        (list (racket struct) (racket define-struct-2))
        (list (racket append) (racket list-append))
        (list (racket map) (racket list-map))
        (list (racket filter) (racket list-filter))
    )
]

Irregularities arise because some names are already taken, such as @racket[define-struct].


@section{TODO: Generic container functions (map, for-each, filter, etc.)}

Those functions should be genericized so that
it works uniformly with lists, vectors, streams, sequences, etc.


@section{Syntaxes, macros}

Also consider using the @racketmodname[syntax/parse] module.

@defform[(define-syntax-rules Name Literals Clause ...)]{

    This expands to

    @racketblock[
        (define-syntax Name (syntax-rules Literals Clause ...))
    ]

    which is like @racket[define-syntax-rule],
    but this allows literals and multiple clauses as in @racket[syntax-rules].
}


@section{Modules, imports, exports}

@defform[(require+provide Module Import ...)]{

    This @racket[require]s the @racket[Import]s from the @racket[Module],
    and @racket[provide]s the imports,
    after optionally renaming some of those imports.

    @racket[Module] is a module reference as accepted by @racket[require].

    @racket[Import] is either a name such as @racketid[original],
    or a list of two names such as @racketid[(original renamed)].

    For example, the fragment

    @racketblock[
        (require+provide Module
            Symbol
            (Original Renamed)
        )
    ]

    expands to the fragment

    @racketblock[
        (begin
            (require (only-in Module
                Symbol
                (Original Renamed)
            ))
            (provide Symbol)
            (provide Renamed)
        )
    ]
}
