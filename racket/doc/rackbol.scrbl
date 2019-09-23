#lang stc-racket/scribble-manual

@(require rackbol)

@title{Rackbol: Racket-based Business-Oriented Language}

@section{About Rackbol}

Rackbol stands for "Racket-based Business-Oriented Language".

The key ideas are:

@itemlist[
    @item{Similar intentions should have similar forms.}
    @item{
        Programmers should specialize into language designers and language users, not into back-end and front-end.
        Programmers should specialize into layers of lasagna, not into pieces of cake.
        Back-end/front-end specialization adds productivity,
        but language-designer/language-user specialization @emph{multiplies} productivity.
        The trade off is... it also multiplies complexity? :(
    }
]

@section{Before you marry us}

Check that your requirements are compatible with our @emph{fundamental assumptions}:

@itemlist[
    @item{The output is a Web application using Racket's "Continue" Web framework.}
    @item{The only supported DBMS is PostgreSQL 9.5 or later.}
    @item{The application is developed on and is deployed to a GNU/Linux installation, especially Debian 9.}
    @item{We may change our tech stack and stop supporting our previous tech stacks.}
    @item{
        We act in our self-interest:
        We do what interests us, the Rackbol developers, not you.
        If it works for us, we are not going to fix it for you.
    }
    @item{Rackbol comes with no support and no warranty. Use it at your own risk.}
    @item{Rackbol is opinionated and inflexible. It is not a general-purpose programming language.}
]

Those fundamental assumptions are hard to change.
If your requirements conflict or will conflict with them,
then do not use this language, unless you are willing to develop the language yourself.

After you make sure that you can afford the fundamental assumptions,
check the user guide and see whether you like this language.
If you don't like it, don't use it.

If you decide to marry us, you will eventually find something you don't like.
When that time comes, you have three choices:

@itemlist[
    @item{accept our shortcomings, and learn to live with the pain; or}
    @item{join us, and help develop Rackbol; or}
    @item{divorce us, and find another language.}
]

@include-section[(submod "rackbol-guide.scrbl" doc)]
@include-section["rackbol-reference.scrbl"]
