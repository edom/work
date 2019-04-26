:- annotate(file,[
    problem-"meta-predicates not handled"
]).

link_unit_clause(F, A:-B, Y:-Z) :- !, A=Y, link_unit_goal(F,B,Z).
link_unit_clause(_, A, Z) :- !, A=Z.

link_unit_goal(F,(A,B),(Y,Z)) :- !, link_unit_goal(F,A,Y), link_unit_goal(F,B,Z).
link_unit_goal(F,(A;B),(Y;Z)) :- !, link_unit_goal(F,A,Y), link_unit_goal(F,B,Z).

link_unit_goal(F,A,Z) :-
    functor(A,Name,Arity),
    file_predicate_origin(F,Name/Arity,local), !,
    Z = A.

link_unit_goal(F,A,Z) :-
    functor(A,Name,Arity),
    file_predicate_origin(F,Name/Arity,module(M)),
    unit_module(F,I),
    !,
    Z = '@'(M:A,I).

link_unit_goal(F,A,_) :- !,
    throw_error(cannot_resolve_goal(F,A)).

    file_predicate_origin(F,P,local) :-
        file_predicate(F,P).

    file_predicate_origin(F,P,module(system)) :-
        file_import(F,system,P).

    file_predicate_origin(F,P,module(user)) :-
        file_import(F,user,P).

    file_predicate_origin(F,P,module(E)) :-
        file_import(F,module(E),P).

    file_predicate_origin(F,P,module(E)) :-
        file_import(F,file(FE),P),
        unit_module(FE,E).

    prolog:error_message(cannot_resolve_goal(F,G)) -->
        {functor(G,N,A)},
        [
            "In ~w:"-[F],nl,
            "Cannot resolve goal: ~w"-[G],nl,
            "because that file does not import that predicate.",nl,
            "Possible fix: Add something like ~w in that file."-[:-import(somewhere,[N/A])]
        ].
