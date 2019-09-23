#lang stc-racket/scribble-manual

@(begin
    (require (only-in scribble/example [examples s_examples]))
    (require racket/pretty)
    (require racket/sandbox)
    (define my_eval
        (parameterize (
                [sandbox-output 'string]
                [sandbox-error-output 'string]
                [sandbox-memory-limit 50]
                [current-print pretty-print-handler]
            )
            [make-evaluator 'racket
                #:requires '(rackbol)
            ]))
    (define-syntax-rule (examples Arg ...)
        (s_examples #:eval my_eval Arg ...)
    )
)

@(require rackbol)

@title{Rackbol reference}

@defmodule[rackbol]

@section{Reexports and shadowings}

From @racketmodname[racket/pretty], we reexport @racket[pretty-print].

From @racketmodname[racket/include], we reexport @racket[include].

We shadow @racket[λ].

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

@defform[#:literals (IN) (LET* name-1 expr-1 ... name-n expr-n IN body ...)]{This is @racket[let*] but with @racket[LET] syntax.}
@defform[#:literals (IN) (LETREC name-1 expr-1 ... name-n expr-n IN body ...)]{This is @racket[letrec] but with @racket[LET] syntax.}
@defform[#:literals (IN) (LETREC* name-1 expr-1 ... name-n expr-n IN body ...)]{This is @racket[letrec*] but with @racket[LET] syntax.}

@section{Definitions}

This tries to capture the ontology of an entire software system.

Every type has its own namespace.
Pro: Shorter names.
Con: Confuses DrRacket.

@defform*[#:literals (
        ACTION
        ADDRESS
        AT
        CATALOG
        DEFAULT
        FOR
        HOST
        HTTP
        INPUT
        LIMIT
        LOG
        MACHINE
        NAME
        OUTPUT
        PASSWORD
        PORT
        POSTGRESQL
        PROCEDURE
        PROTOCOL
        RATE
        SCHEMA
        SERVE
        SERVER
        STORAGE
        TABLE
        THIS
        TO
        TYPE
        USER
        VALUE
    ) [
        (DEFINE MACHINE machine-id machine-parameter ...)
        (DEFINE PROCEDURE procedure-id procedure-parameter ...)
        (DEFINE SERVER server-id server-parameter ...)
        (DEFINE STORAGE storage-id storage-parameter ...)
        (DEFINE TABLE table-id table-parameter ...)
        (DEFINE VALUE value-id expression)
    ]
    #:grammar [
        (procedure-parameter
            (INPUT input-id input-parameter ...)
            (OUTPUT output-id output-parameter ...)
            (ACTION expression ...)
        )
        (input-parameter
            (code:line NAME input-name)
            (code:line TYPE input-type)
            (code:line DEFAULT input-default (code:comment "optional"))
        )
        (output-parameter
            (code:line NAME output-name)
            (code:line TYPE output-type)
        )
        (machine-parameter
            (code:line THIS MACHINE)
            (code:line ADDRESS ip-address-slash-subnet (code:comment "zero or more"))
        )
        (server-parameter
            (code:line HOST machine-id)
            (code:line PORT port)
            (code:line PROTOCOL HTTP)
            (code:line LOG TO log-file-path)
            (code:line LIMIT RATE TO count PER MINUTE PER IP ADDRESS)
            (SERVE procedure AT url-path FOR authorization)
        )
        (storage-parameter
            [code:line TYPE POSTGRESQL]
            [code:line HOST host]
            [code:line PORT port]
            [code:line CATALOG catalog]
            [code:line USER user]
            [code:line PASSWORD password]
        )
        (table-parameter
            [code:line STORAGE storage-id]
            [code:line SCHEMA schema]
            [code:line NAME name]
        )
    ]
]{
    Most values are quasiquoted by default because, in our experience,
    most values fed to DEFINE are constant literals.

    In Rackbol, @emph{things must be defined before they are used/referenced}.

    Design Question:
    Should Rackbol be a Racket language module instead of a Racket library module?
    If so, we could use @racket[#%module-begin] to reorder definitions to the beginning.
    But there seems to be no way to make a
    @racketmodname[scribble/lp2] main chunk that begins with a #lang,
    because those #lang directives don't compose,
    although #reader may provide some workaround.
}

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

    BY HASH KEY requires that each element of @racket[input] be a hash table.

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

    @examples[
        (define messages [list
            (hash 'id 0 'sender 'joe)
            (hash 'id 1 'sender 'bob)
            (hash 'id 2 'sender 'joe)
            (hash 'id 3 'sender 'joe)
            (hash 'id 4 'sender 'bob)
            (hash 'id 5 'sender 'alice)
        ])
        (code:comment "equivalent")
        (GROUP LIST messages BY HASH KEY sender)
        (code:comment "equivalent")
        (GROUP LIST messages BY FUNCTION (λ e => hash-ref e 'sender))
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
