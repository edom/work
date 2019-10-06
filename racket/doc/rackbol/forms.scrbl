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

@title{Rackbol forms reference}

@defmodule[rackbol #:lang]

@section{The GROUP form}

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

@section{The λ (lambda) form}

@defform*[#:literals (-> =>) [
    (λ param-1 ... param-n -> body ...)
    (λ param-1 ... param-n => proc arg ...)
]]{
    This is @racket[lambda] but with less parentheses.

    The @racket[=>] (double-arrow) variant implicitly parenthesizes the body.

    The translation scheme is:

    @racketblock[
        (λ param-1 ... param-n -> body ...)
        (code:comment "translates to")
        (lambda (param-1 ... param-n) body ...)

        (λ param-1 ... param-n => proc arg ...)
        (code:comment "translates to")
        (lambda (param-1 ... param-n) (proc arg ...))
    ]

    On Debian, you can type the lambda symbol by pressing the key sequence "Ctrl+Shift+U 3 b b space".

    Our λ clashes with the λ exported by racket/base and racket.
    However, both our λ and their λ produce the same lambda value.
}

@section{The LET form and its variants}

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

@section{The DEFINE form}

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
        PROCEDURE
        PROTOCOL
        RATE
        SCHEMA
        SERVE
        SERVER
        TABLE
        THIS
        TO
        TYPE
        USER
    ) [
        (DEFINE MACHINE machine-id machine-parameter ...)
        (DEFINE PROCEDURE procedure-id procedure-parameter ...)
        (DEFINE SERVER server-id server-parameter ...)
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
    ]
]{
    Design Question:
    Should Rackbol be a Racket language module instead of a Racket library module?
    If so, we could use @racket[#%module-begin] to reorder definitions to the beginning.
    But there seems to be no way to make a
    @racketmodname[scribble/lp2] main chunk that begins with a #lang,
    because those #lang directives don't compose,
    although #reader may provide some workaround.
}

@section{The DEFINE PROCEDURE form}

@defform[
    #:link-target? #f
    #:literals (PROCEDURE INPUT)
    (DEFINE PROCEDURE id input ... body ...)
    #:grammar [
        (input
            [code:line INPUT input-id]
        )
    ]
]{
    A procedure named "Proc" can be translated to one HTML form in the URL path "/Proc" with both GET and POST method,
    in which each procedure input translates to a HTML input control.
}

@section{The DEFINE STORAGE form}

@defform[
    #:link-target? #f
    #:literals (STORAGE TYPE postgresql HOST PORT CATALOG USER PASSWORD)
    (DEFINE STORAGE id TYPE postgresql postgresql-parameter ...)
    #:grammar [
        (postgresql-parameter
            [code:line HOST host]
            [code:line PORT port]
            [code:line CATALOG catalog]
            [code:line USER user]
            [code:line PASSWORD password]
        )
    ]
]{
    @racket[host] is the @racket[quasiquote]d host.

    @racket[port] is the @racket[quasiquote]d port.
    For PostgreSQL, this is usually 5432.

    @racket[catalog] is the @racket[quasiquote]d catalog name.

    @racket[user] is the @racket[quasiquote]d user for login.

    @racket[password] is the @racket[quasiquote]d password for login.
}

@section{The DEFINE TABLE form}

@defform[
    #:link-target? #f
    #:literals (TABLE STORAGE SCHEMA NAME)
    (DEFINE TABLE id parameter ...)
    #:grammar [
        (parameter
            [code:line STORAGE storage-id]
            [code:line SCHEMA schema]
            [code:line NAME name]
        )
    ]
]{
    @racket[storage-id] is an identifier defined by a DEFINE STORAGE form.

    @racket[schema] is the @racket[quasiquote]d name of the schema that contains the table.

    @racket[name] is the @racket[quasiquote]d name without schema name.
}
