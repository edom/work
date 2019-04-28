:- section("check").

    my_check :-
        forall(unit(Unit),
            print_unit_errors(Unit)).

    print_unit_errors(Unit) :-
        forall(unit_error(Unit,Error), (
            print_message(error, Error),
            nl
        )).

    unit_error(U, unit_calls_undefined_predicate(U,H,P,O)) :-
        unit(U),
        setof(e(H,P,O), unit_goalpred(U,H,P,O), Ps),
        member(e(H,P,O), Ps),
        \+ unit_predicate_module(U, P, _).

    unit_error(I, unit_imports_unexported_predicate(I,E,P)) :-
        object_import(I,E,P),
        \+ object_export(E,P).

        unit_goalpred(Unit, Head, Pred, Origin) :-
            unit_goal(Unit, Head, Goal, Origin),
            goal_pred(Goal, Pred).

        unit_goal(Unit, Head, Goal, Origin) :-
            unit_clause(Unit, _, Head :- Body, Origin),
            body_goal(Body, Goal),
            (var(Goal) ->
                throw_error(unit_has_variable_goal_in_clause_body(Unit,Head,Goal,Origin))
            ;   true
            ).

            body_goal(A, Z) :- var(A), !, A = Z.
            body_goal((A,B), Z) :- !, (body_goal(A,Z) ; body_goal(B,Z)).
            body_goal((A;B), Z) :- !, (body_goal(A,Z) ; body_goal(B,Z)).
            body_goal(A, Z) :- !, A = Z.

:- end_section.
