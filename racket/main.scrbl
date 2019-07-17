#lang scribble/manual

@;  How do we make our own #lang so that we don't have to repeat typing this "require"?

@(require
    "scribble-more.rkt"
    (for-label
        racket
        "scribble-more.rkt"
    )
)

@;  --------------------    Contents.

@title{Erik Dominikus's Racket stuffs}
@author{Erik Dominikus}

@table-of-contents[]

@include-section["idea.scrbl"]

@section{Documenting Racket code}

Follow the Scribble documentation@footnote{@url{https://docs.racket-lang.org/scribble/index.html}}.

@section{Looking for the authoring system}

These facts sway my opinions.

I am comparing: Scribble, Pollen, Org-Mode.

Org-Mode is the only reason I use Emacs.

Scribble can do BibTeX.
@footnote{@url{https://docs.racket-lang.org/scribble/Bibliography.html}}
@footnote{@url{https://docs.racket-lang.org/scriblib/autobib.html}}
@footnote{@url{https://docs.racket-lang.org/scriblib/bibtex.html}}

I find Noto Sans much more legible than Scribble's serif font.
