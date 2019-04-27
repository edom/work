link_unit_clause(F, A:-B, Y:-Z) :- !,
    A = Y,
    link_unit_goal(F,B,Z).

link_unit_clause(_, A, Z) :- !,
    A = Z.

    :- annotate([
        problem-"meta-predicates not handled"
    ]).

    % ,/2 and ;/2 are actually system predicates.

    link_unit_goal(F, (A,B), (Y,Z)) :- !,
        link_unit_goal(F, A, Y),
        link_unit_goal(F, B, Z).

    link_unit_goal(F, (A;B), (Y;Z)) :- !,
        link_unit_goal(F, A, Y),
        link_unit_goal(F, B, Z).

    link_unit_goal(F, Goal, Z) :-
        functor(Goal, Name, Arity),
        unit_predicate_module(F, Name/Arity, Exporter),
        !,
        get_unit_module_or(F, Importer, throw),
        Z = '@'(Exporter:Goal, Importer).

    link_unit_goal(F, A, _) :- !,
        throw_error(cannot_link_goal(F,A)).

        prolog:error_message(cannot_link_goal(F,G)) -->
            {functor(G,N,A)},
            [
                "In file: ~w"-[F],nl,
                "In goal: ~p"-[G],nl,
                "The problem: Undefined predicate: ~p"-[N/A],nl,
                "Solutions:",nl,
                "- If you want to import ~p from somewhere else:"-[N/A],nl,
                "  - Add an import like ~p in that file."-[:-import(somewhere,[N/A])],nl,
                "- If you feel that this should not happen:",nl,
                "  - If you believe that ~p is builtin:"-[N/A],nl,
                "    - Add a default_import/2 clause in import.pro for ~p."-[N/A],nl,
                "  - Otherwise:",nl,
                "    - Check for mistakes in spelling, argument counts, and parentheses.",nl,
                "    - Have you defined the predicate?"
            ].

    :- annotate([meaning="the Arg-th argument of Goal is module-sensitive"]).
    module_sensitive(Goal, Arg) :-
        predicate_property(Goal, meta_predicate(Meta)),
        arg(Arg, Meta, M),
        (integer(M) ; M = ':').

:- annotate([purpose="compute the module of an unqualified goal in a unit"]).
unit_predicate_module(Unit, Pred, Module) :-
    unit_predicate(Unit, Pred),
    get_unit_module_or(Unit, Module, throw).

unit_predicate_module(Unit, Pred, system) :-
    unit_import(Unit, system, Pred).

unit_predicate_module(Unit, Pred, user) :-
    unit_import(Unit, user, Pred).

unit_predicate_module(Unit, Pred, Mod) :-
    unit_import(Unit, module(Mod), Pred).

unit_predicate_module(Unit, Pred, Mod) :-
    unit_import(Unit, unit(FE), Pred),
    unit_module(FE, Mod).
