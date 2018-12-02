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


% experimental

expression_normalized(A,A) :- var(A), !.
expression_normalized([],[]) :- !.
expression_normalized([H|T], [NH|NT]) :- !, expression_normalized(H,NH), expression_normalized(T,NT).
expression_normalized(E,N) :-
    E =.. [F|Args],
    expression_normalized(Args, NArgs),
    E1 =.. [F|NArgs],
    (expression_expansion(E1,N) -> true ; E1 = N).

expression_expansion(A+B, C) :- number(A), number(B), !, C is A+B.
expression_expansion(A+B, C) :- string(A), string(B), !, string_concat(A,B,C).
expression_expansion(strlen(S), N) :- string(S), !, string_length(S,N).
