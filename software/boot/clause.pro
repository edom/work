/*
A clause is a goal or a term like Head :- Body.

A goal is something like a:b(c,d) or a(b,c).

A pred, a predicate reference, or a pred_ref,
is a Name/Arity (unqualified predicate reference)
or a Module:Name/Arity (qualified predicate reference).
*/

:- section("term to clause").

    :- annotate([purpose="translate a read term to a clause, or fail"]).
    term_clause(:- _, _) :- !, fail.
    term_clause(A :- B, Z) :- !, Z = (A :- B).
    term_clause(A --> B, Z) :- !, dcg_clause(A --> B, Z).
    term_clause(A, Z) :- !, Z = A.

    clause_head(A :- _, Z) :- !, A = Z.
    clause_head(A, Z) :- !, A = Z.

    goal_pred(Module:Func, Pred) :- !,
        Pred = Module:Name/Arity,
        functor(Func, Name, Arity).

    goal_pred(Goal, Pred) :- !,
        Pred = Name/Arity,
        functor(Goal, Name, Arity).

:- end_section.


:- section("expand definite-clause-grammar").

    %   '$my_phrase'/3 should be compatible with phrase/3.

    '$my_phrase'(A,I,K) :-
        dcg_goal(A,I,K,Z),
        call(Z).

    dcg_clause(A --> B, Y :- Z) :- !,
        add_goal_args(A,[I,J],Y),
        dcg_goal(B,I,J,Z).

    %%  dcg_goal(+DcgGoal, -OrdinaryGoal) is det.

    dcg_goal(A,I,K,Z) :- var(A), !,
        Z = '$my_phrase'(A,I,K).

    dcg_goal((A,B),I,K,Z) :- !,
        Z = (X,Y),
        dcg_goal(A,I,J,X),
        dcg_goal(B,J,K,Y).

    dcg_goal((A;B),I,K,Z) :- !,
        Z = (X;Y),
        dcg_goal(A,I,K,X),
        dcg_goal(B,I,K,Y).

    dcg_goal((A->B),I,K,Z) :- !,
        Z = (X->Y),
        dcg_goal(A,I,J,X),
        dcg_goal(B,J,K,Y).

    dcg_goal((\+A),I,K,Z) :- !,
        Z = (\+Y),
        dcg_goal(A,I,K,Y).

    dcg_goal(!,I,K,Z) :- !,
        Z = !,
        I = K.

    dcg_goal({A},I,K,Z) :- !,
        Z = A,
        I = K.

    dcg_goal(A,I,K,Z) :- is_list(A), !,
        Z = append(A,K,I).

    dcg_goal(A,I,J,Z) :- string(A), !,
        string_codes(A,B),
        dcg_goal(B,I,J,Z).

    dcg_goal(A,I,K,Z) :- !,
        add_goal_args(A,[I,K],Z).

    %%  add_goal_args(+Goal, +ExtraArgs, -GoalWithExtraArgs) is det.
    %
    %   Goal may be module-qualified.
    %
    %   ExtraArgs is a list.

    add_goal_args(A, _, _) :- var(A), !, instantiation_error(A).
    add_goal_args(_, A, _) :- var(A), !, instantiation_error(A).

    add_goal_args(A, Args, Z) :-
        A = Module:B, !,
        Z = Module:Y,
        add_goal_args(B, Args, Y).

    add_goal_args(A, Args, Z) :- !,
        A =.. List1,
        append(List1, Args, List2),
        Z =.. List2.

:- end_section.
