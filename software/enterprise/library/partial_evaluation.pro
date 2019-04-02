% Use with include/1.

%%  should_call(?Goal) is nondet.
%   The provability of should_call(Goal) means that the partial evaluator
%   should assume that the Goal is pure and deterministic.
%   "Pure" means monotonic and free of side effects.

:- discontiguous should_call/1.

%   should_expand should only be used if you are sure that there is at most one matching clause.

:- discontiguous should_expand/1.

%%  partial_evaluation(+Exp1, -Exp2) is det.
%   Partially evaluate Exp1 to Exp2.

partial_evaluation(A, Z) :- var(A), !, A = Z.
partial_evaluation(false, Z) :- !, Z = false.
partial_evaluation(true, Z) :- !, Z = true.
partial_evaluation((false, _), Z) :- !, Z = false.
partial_evaluation((true, A), Z) :- !, partial_evaluation(A, Z).
partial_evaluation((A, true), Z) :- !, partial_evaluation(A, Z).
partial_evaluation((A,B), Z) :- partial_evaluation(A,A0), A \== A0, !, partial_evaluation((A0,B), Z).
partial_evaluation((A,B), Z) :- partial_evaluation(B,B0), B \== B0, !, partial_evaluation((A,B0), Z).
partial_evaluation(A, Z) :- should_call(A), !, (call(A) -> Z = true ; Z = false).
partial_evaluation(A, Z) :- should_expand(A), !, clause(A, B), !, partial_evaluation(B, Z).
partial_evaluation(A, Z) :- !, A = Z.
