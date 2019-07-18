#lang stc-racket/scribble-manual

@(define (fnurl x) (footnote (url x)))

@;  --------------------    Contents.

@title{Erik Dominikus's Racket stuffs}
@author{Erik Dominikus}

@table-of-contents[]


@include-section["stc-racket.scrbl"]


@section{Documenting Racket code}

Follow the Scribble documentation@footnote{@url{https://docs.racket-lang.org/scribble/index.html}}.

I prefer separate rkt and scrbl file.


@section{Looking for the authoring system}

I am comparing: Scribble, Pollen, Org-Mode.

Org-Mode is the only reason I use Emacs.

Scribble can do BibTeX.
@footnote{@url{https://docs.racket-lang.org/scribble/Bibliography.html}}
@footnote{@url{https://docs.racket-lang.org/scriblib/autobib.html}}
@footnote{@url{https://docs.racket-lang.org/scriblib/bibtex.html}}

Scribble can do LaTeX math@footnote{@url{https://docs.racket-lang.org/scribble-math/index.html}},
but the syntax is sub-optimal.

I hate Scribble's serif font.
I find Noto Sans much more legible.

Scribble can also be used to write blogs.
@footnote{@url{https://docs.racket-lang.org/scriblogify/index.html}}
But I'm already using Org-Mode.

I have invested a lot in Org-Mode.
Much sunk cost.

2019-07-19:
Scribble problem:
How do we number footnotes in Scribble HTML output?


@section{Writing languages in Racket}

@racket[syntax/parse] is a must.
@footnote{@url{https://docs.racket-lang.org/syntax/index.html?q=syntax%20parse}}


@section{Web applications}

I am not using Racquel@fnurl{https://github.com/brown131/racquel} because its license is GPL.
But I read its documentation@fnurl{https://docs.racket-lang.org/racquel/index.html}.
