% Imperative programming in Prolog.

:- meta_predicate deterministically(0).

deterministically((A,B)) :- !,
    deterministically(A),
    deterministically(B).

deterministically(A) :- A -> true ; throw_error(should_not_fail(A)).

throw_error(E) :- throw(error(E,_)).

prolog:error_message(should_not_fail(Goal)) -->
    ['Goal should not fail: ~w\n'-[Goal]].
