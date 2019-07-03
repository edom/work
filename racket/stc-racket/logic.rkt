;;  logic programming

(module stc-racket-logic racket/base

    (require racklog)

    (provide
        (rename-out
            (%assert!           %asserta!)
            (%assert-after!     %assertz!)
        )

        %true
        (rename-out
            (%fail              %false)
        )

        %/=
        %=

        %/==
        %<
        %<=
        %=/=
        %=:=
        %==
        %>
        %>=

        %and
        %if-then-else
        %is
        %not
        %or

        %append
        %member

        %empty-rel
        %more
        %rel
        %which

        %nonvar
        %var

        %bag-of
        %set-of
    )
)