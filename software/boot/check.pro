my_check :-
    forall(unit_error(U,E), print_message(error,unit_error(U,E))).

unit_error(U, undefined_predicate(P)) :-
    unit(U),
    setof(P, unit_goalpred(U,P), Ps),
    member(P, Ps),
    \+ unit_predicate_module(U,P,_).

unit_error(I, importing_unexported_predicate(E,P)) :-
    object_import(I,E,P),
    \+ object_export(E,P).

    unit_goalpred(U, N/A) :-
        unit_goal(U, G),
        functor(G, N, A).

    unit_goal(U, G) :-
        unit_clause(U, _, _ :- B),
        body_goal(B, G),
        must_be(nonvar, G). % wrap the variable in call/1 first

        body_goal((A,B), Z) :- !, (body_goal(A,Z) ; body_goal(B,Z)).
        body_goal((A;B), Z) :- !, (body_goal(A,Z) ; body_goal(B,Z)).
        body_goal(A, Z) :- !, A = Z.

prolog:message(unit_error(U,undefined_predicate(P))) -->
    ["In unit ~w: Undefined predicate: ~w"-[U,P]].

prolog:message(unit_error(I,importing_unexported_predicate(E,P))) --> [
    "In ~p:"-[I],nl,
    "Error: Importing unexported predicate",nl,
    "Predicate: ~p"-[P],nl,
    "Exporter: ~p"-[E]
].
