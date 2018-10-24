/*
An expression sublanguage to facilitate functional-logic programming in Prolog.
*/
:- module(expression, [
    expression_value/2
]).


/*
Embedded C-like expression language but with unification instead of assignment.

The expression fun(Input) is translated to the goal/phrase fun(Input, Result).

Example usage:

expression_value((
    A = 1;
    (B, C) = (2, A + 2 + 3);
    D = 4 + 5 * sin(6);
    E = 7 + plus(8,9)
), Result).
*/

% Haskell list append.
:- op(650, xfy, ++).

eval_arith_binop(E, Val) :-
    E =.. [Name, EA, EB],
    expression_value(EA, VA),
    expression_value(EB, VB),
    Exp =.. [Name, VA, VB],
    Val is Exp.

expression_value(A, A) :-
    atom(A), !
;   number(A), !
;   string(A), !.

% This case is problematic.
% We can't have good error message.
expression_value(V, V) :- var(V), !.

expression_value([], []) :- !.
expression_value([H|T], [H|T]) :- !.

expression_value((EA, EB), (EA, EB)) :- !.

expression_value(E, Val) :-
    functor(E, Name, 2),
    member(Name, [+, *]),
    !,
    eval_arith_binop(E, Val).

expression_value((EA ++ EB), Val) :-
    !,
    expression_value(EA, VA),
    expression_value(EB, VB),
    append(VA, VB, Val).

expression_value((Pat = E), Val) :-
    !,
    expression_value(E, Val),
    Pat = Val.

expression_value((EA; EB), Val) :-
    !,
    expression_value(EA, _),
    expression_value(EB, Val).

expression_value(Exp, Val) :-
    current_arithmetic_function(Exp),
    !,
    Val is Exp.

expression_value(Exp, Val) :-
    functor(Exp, _, _),
    !,
    call(Exp, Val).
