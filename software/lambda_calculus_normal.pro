:- module(lambda_calculus_normal, [
    exp_val/2
    , exp_reduce/2
    , name_rep_exp_sub/4
    , subst/4
]).
/** <module> untyped lambda calculus with normal-order reduction strategy

An expression is any of these:
    - con(Val) represents a constant that evaluates to Val.
    - var(Name) represents a variable whose name is the atom Name.
    - lam(Name,Body) represents a lambda abstraction where Name is an atom that represents the variable name.
    Note that Name is an atom, not a compound var(C).
    - app(Lam,Arg) represents a lambda application.
*/

/** exp_val(+Exp, -Val).

"Evaluate (normalize) Exp to Val."
*/
exp_val(A,B) :-
    exp_reduce(A,RA) -> exp_val(RA,B) ; A=B.

/** exp_reduce(+Exp1, -Exp2).

"One step of normal-order beta reduction transforms Exp1 to Exp2."
*/
:- multifile exp_reduce/2.
exp_reduce(con(A), con(A)).
exp_reduce(var(A), var(A)).
exp_reduce(app(lam(V,B),A), Sub) :- name_rep_exp_sub(V, A, B, Sub).
exp_reduce(app(A,B), app(RA,B)) :- exp_reduce(A, RA).

/** name_rep_exp_sub(+Name, +Rep, +Exp, -Sub).

"Substitute Name with Rep in Exp, producing Sub."

Replace every free occurrence of Name in Exp with Rep, producing Sub.
*/
name_rep_exp_sub(_, _, con(A), con(A)).
name_rep_exp_sub(Name, Rep, var(Name), Rep).
name_rep_exp_sub(Name, _, lam(Name,Body), lam(Name,Body)).
name_rep_exp_sub(Name, Rep, lam(N,Body1), lam(N,Body2)) :- N \= Name, name_rep_exp_sub(Name, Rep, Body1, Body2).
name_rep_exp_sub(Name, Rep, app(Lam1,Arg1), app(Lam2,Arg2)) :-
    name_rep_exp_sub(Name, Rep, Lam1, Lam2),
    name_rep_exp_sub(Name, Rep, Arg1, Arg2).

/** subst(+Needle, +Rep, +Haystack, -Out).

"Out is Haystack with every occurrence of Needle substituted with Rep."

This is an alias of name_rep_exp_sub/4 with shorter name.
*/
subst(A,B,C,D) :- name_rep_exp_sub(A,B,C,D).
