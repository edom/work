#lang stc-racket/scribble-manual

@(require "../stc-racket/stc-rackbol.rkt")

@title{STC-Rackbol reference}

@defmodule["../stc-racket/stc-rackbol.rkt"]

@section{Racket with fewer parentheses}

You can write fewer parentheses with @racket[λ], @racket[LET], and its variants.
But we don't aim to eliminate parentheses completely.
This is just a minor usability enhancement.

@defform*[#:literals (-> =>) [
    (λ param-1 ... param-n -> body ...)
    (λ param-1 ... param-n => proc arg ...)
]]{
    This is Haskell-like lambda-expression syntax but without currying and without infix syntax.

    The double-arrow variant implicitly parenthesizes the body.

    The above forms are respectively equivalent to:

    @racketblock[
        (lambda (param-1 ... param-n) body ...)
        (lambda (param-1 ... param-n) (proc arg ...))
    ]

    On Debian, you can type the lambda symbol by pressing the key sequence "Ctrl+Shift+U 3 b b space".
}

@defform[#:literals (IN)
        (LET name-1 expr-1 ... name-n expr-n IN body ...)]{
    This is @racket[let] but with fewer parentheses.

    This reserves the symbol @racket[IN].

    Equivalent to:

    @racketblock[
        (let [
                (name-1 expr-1)
                ...
                (name-n expr-n)
            ]
            body ...
        )
    ]

    Example:

    @racketblock[
        (LET [
                a   1
                b   2
                c   3
            ] IN [+ a b c])
    ]
}

@defform[(LET* name-1 expr-1 ... name-n expr-n body ...)]{This is @racket[let*] but with @racket{LET} syntax.}
@defform[(LETREC name-1 expr-1 ... name-n expr-n body ...)]{This is @racket[letrec] but with @racket{LET} syntax.}
@defform[(LETREC* name-1 expr-1 ... name-n expr-n body ...)]{This is @racket[letrec*] but with @racket{LET} syntax.}

@section{Common data operations}

@racket[GROUP].

@defform*[#:literals (BY DICT FUNCTION HASH IN KEY LIST VECTOR WITH)
    [
        (GROUP source by)
        (GROUP WITH KEY property-name IN groups)
    ]
    #:grammar
    [
        (source
            [code:line LIST input]
            [code:line VECTOR input]
        )
        (by
            [code:line BY FUNCTION key-function]
            [code:line BY DICT KEY property-name]
            [code:line BY HASH KEY property-name]
        )
    ]
]{
    GROUP groups/partitions the input collection according to @racket[equal?] keys.

    GROUP WITH KEY gets the group with the given key,
    where @racket[groups] is the result of a GROUP statement.

    @racket[key-function] must be a procedure that
    takes an element of the input collection and gives its grouping key.

    @racket[property-name] will automatically be wrapped in @racket[quasiquote].

    Notes for forward compatibility:

    @itemlist[
        @item{
            You must not assume that GROUP produces a hash table.
            Always use GROUP WITH KEY to access the groups.
            Never use @racket[hash-ref] directly with the groups.
        }
        @item{
            You must not depend on the ordering of the elements in the returned list.
            If you want them in a particular order, you must order them separately.
        }
    ]

    Example:

    @racketblock[
        (define messages [list
            (hash 'id 0 'sender 'joe)
            (hash 'id 1 'sender 'bob)
            (hash 'id 2 'sender 'joe)
            (hash 'id 3 'sender 'joe)
            (hash 'id 4 'sender 'bob)
            (hash 'id 5 'sender 'alice)
        ])
        (GROUP LIST messages BY FUNCTION (λ e => hash-ref e 'sender))
        (GROUP LIST messages BY HASH KEY sender)
    ]

    Design Question:
    We use GROUP as both a verb and a noun.
    Is this confusing?
    Is this bad design?
}

@section{Operations}

After you develop software, you operate it.

@subsection{Database operations}

@defproc[(psql [target any/c]) void?]{
    Open a psql shell to the target database.

    This requires the external program /usr/bin/psql.

    @racket[target] is a database specification.

    Security Problem:
    Because this function uses the PGPASSWORD environment variable,
    the password leaks in the file /proc/<pid>/environ, which is readable by the user that psql is running as,
    which is the user that your Racket interpreter runs as, which is your user.
    Thus, practically all other running programs can read the password.
}
