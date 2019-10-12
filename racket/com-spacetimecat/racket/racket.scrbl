#lang stc-racket/scribble-manual

@;  --------------------    Contents.

@title{Erik Dominikus's Racket stuffs}
@author{Erik Dominikus}


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

Scribble can also be used to write blogs.
@footnote{@url{https://docs.racket-lang.org/scriblogify/index.html}}
But I'm already using Org-Mode.

I have invested a lot in Org-Mode.
Much sunk cost.

2019-07-19:
Scribble problem:
How do we number footnotes in Scribble HTML output?


@section{Writing languages in Racket}

The @racketmodname[syntax/parse] module...


@include-section["webapp.scrbl"]


@section{Optimization}

Typed Racket's optimizer uses @racketmodname[syntax/parse].
@fnurl{https://www.youtube.com/watch?v=mtR3NupaRAQ}

How does that compare to Nanopass?


@section{Org-Mode syntax for Scribble}

So that we can use Emacs for editing large text documents with many sections.
We can easily move trees in Org-Mode.

Or just use Skribilo?@fnurl{https://www.nongnu.org/skribilo/}

Or port Skribilo to Racket?

Or make Emacs Lisp a Racket language?
@fnurl{Emacs Lisp as a Racket Language? https://groups.google.com/forum/#!topic/racket-users/Bx-E_Oqf_d0}
@fnurl{https://github.com/tonyg/rmacs}


@section{Haskell syntax for Racket}

It is quite tiring, typing all those "define"s and parentheses in Racket.

Haskell syntax is clean.

- Haskell syntax, Racket semantics; Racket reader for Haskell syntax.
- Strict instead of lazy.
- Replace Template Haskell with Racket macros.
- Replace Haskell modules with Racket modules.
- Parse but ignore all type annotations.
- Custom infix operators, with precedence, but not as flexible as Prolog.
- Currying?
- No keyword arguments? But we have keyword-apply?
- Using reserved keywords as normal keywords, such as [r|in]?
- Layout-sensitive, but can also be layout-insensitive with explicit curly braces and semicolons.
- Minimize the pain of switching from Haskell to Racket.

@verbatim|{
module Mymod where -- optional; ignored?

--  Splice fragments into the corresponding Racket module body.
[racket|
(define foo 123)
|]

f x y = x + y*y

g = [1,2,3]
}|

translates to

@verbatim|{
(module Mymod racket/base
    (define foo 123)
    (define (f x y) (+ x (* y y)))
    (define g '(1 2 3))
)
}|


@section{Database}

The database should be a library, not a server.

Back-up should be as easy as adding a node.
Replication should be automatic.
