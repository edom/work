#lang stc-racket/scribble-manual

@title{The STC-Racket language}

@defmodule[stc-racket #:lang]

There has not yet been any strong reason to use STC-Racket.

The STC-Racket language is just Racket with some renamings for consistency.

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

Some irregularities arise because some names are already taken, such as @racket[define-struct].

On second thought, perhaps we should shorten @racket[list-map] to @racket[map],
because, in practice, lists seem to be used more often.
