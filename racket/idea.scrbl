#lang scribble/manual

@(require
    "scribble-more.rkt"
    (for-label
        racket
        "scribble-more.rkt"
    )
)

@title{Some ideas}

@section{The STC-Racket language}

There has not yet been any strong reason to use STC-Racket.

The STC-Racket language is just Racket with some renamings for consistency.

@table[
    #:header (list "Racket" "STC-Racket")
    #:caption "Renamings from Racket to STC-Racket"
    #:rows `(
        (,(racket module) "define-module")
        (,(racket struct) "define-struct-2")
        (,(racket map) "list-map")
    )
]

On second thought, perhaps we should shorten "list-map" to "map",
because, in practice, lists seem to be used more often.
