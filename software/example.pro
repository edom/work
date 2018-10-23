:- use_module('data.pro').
:- use_module(library(clpfd)).

entity(todo, [
        [id, int32]
        , [text, varchar(1024)]
        , [what, int32, nullable]
    ]).

main(Class, Ast) :- java_class_ast(Class, Ast).

:- use_module('java_syntax.pro').

test_java_syntax(Tree) :- true
    , string_codes("1 + 2", Source)
    , phrase(expression(Tree), Source, [])
    .


bit(0).
bit(1).

bfs([]).
bfs([H|T]) :- bfs(T), bit(H).

dfs([]).
dfs([H|T]) :- bit(H), dfs(T).

/*
Prolog can handle left-recursive grammar if we ground terms as much as possible before recursing.
*/

% S ::= <empty> | S a

word([]).
word(A) :- append(B, [a], A), word(B).

% S ::= <empty> | S T S
% T ::= a

word2([]).
word2(S) :- t(T), append([S0,T,S1], S), word2(S0), word2(S1).

t([a]).

/*
Expression parsing with precedence/order.

U = unparsed
P = parsed
*/

% Read one character code from the head.

code(P, [P|U], [P], U).

digit(M, U0, P0, U1) :- code(C, U0, P0, U1), code_type(C, digit), M #= C - 0'0.

number(M, U0, P2, U2) :-
    append(P2, U2, U0),
    append(P0, P1, P2),
    digit(MH, U1, P1, U2),
    number(MT, U0, P0, U1),
    M is 10 * MT + MH.

number(M, U0, P0, U1) :- digit(M, U0, P0, U1).


name(M, U0, P2, U2) :-
    name_head(MH, U0, P0, U1),
    append(P0, P1, P2),
    name_tail(MT, U1, P1, U2),
    atom_codes(M, [MH|MT]).

name_head(M, U0, P0, U1) :- code(M, U0, P0, U1), code_type(M, csymf).

name_tail([MH|MT], U0, P2, U2) :-
    code(MH, U0, P0, U1),
    code_type(MH, csym),
    append(P0, P1, P2),
    name_tail(MT, U1, P1, U2).

name_tail([], U0, [], U0).

/*
An order is an integer.

Expression with smaller order is evaluated earlier.

Problem:
Expression with equal order should be parsed from left to right, but this isn't implemented.
*/

expression(200, name(M), U0, P0, U1) :- name(M, U0, P0, U1).
expression(200, number(M), U0, P0, U1) :- number(M, U0, P0, U1).

expression(Order, paren(M), U0, P3, U3) :-
    Order #= 100,
    append(P3, U3, U0),
    append([P0, P1, P2], P3),
    code(0'(, U0, P0, U1),
    code(0'), U2, P2, U3),
    OP #>= Order,
    expression(OP, M, U1, P1, U2).

expression(Order, infix(P1, ML, MR), U0, P3, U3) :-
    Order #= 400,
    append(P3, U3, U0),
    append([P0, P1, P2], P3),
    code(0'+, U1, P1, U2),
    OL #=< Order,
    OR #=< Order,
    expression(OL, ML, U0, P0, U1),
    expression(OR, MR, U2, P2, U3).

expression(Order, infix(P1, ML, MR), U0, P3, U3) :-
    Order #= 400,
    append(P3, U3, U0),
    append([P0, P1, P2], P3),
    code(0'-, U1, P1, U2),
    OL #=< Order,
    OR #=< Order,
    expression(OL, ML, U0, P0, U1),
    expression(OR, MR, U2, P2, U3).

expression(Order, infix(P1, ML, MR), U0, P3, U3) :-
    Order #= 300,
    append(P3, U3, U0),
    append([P0, P1, P2], P3),
    code(0'*, U1, P1, U2),
    OL #=< Order,
    OR #=< Order,
    expression(OL, ML, U0, P0, U1),
    expression(OR, MR, U2, P2, U3).

:- use_module('tax.pro').

example_tax(Gross_income, Tax) :-
    taxation_subject(Taxation, Subject),
    taxation_year(Taxation, 2018),
    subject_has_npwp(Subject, true),
    subject_gross_income(Subject, Income),
    income_year(Income, 2018),
    income_amount(Income, Gross_income),
    taxation_income_tax(Taxation, Tax).

/*
Embedded C-like expression language but with unification instead of assignment.

Translate the expression fun(Input) to the goal fun(Input, Result).

Example query:

eval((
    A = 1;
    (B, C) = (2, A + 2 + 3);
    D = 4 + 5 * sin(6);
    E = 7 + plus(8,9)
), Result).
*/

:- op(650, xfy, ++).

eval_arith_binop(E, Val) :-
    E =.. [Name, EA, EB],
    eval(EA, VA),
    eval(EB, VB),
    Exp =.. [Name, VA, VB],
    Val is Exp.

eval(A, A) :-
    atom(A), !
;   number(A), !
;   string(A), !.

% This case is problematic.
% We can't have good error message.
eval(V, V) :- var(V), !.

eval([], []) :- !.
eval([H|T], [H|T]) :- !.

eval((EA, EB), (EA, EB)) :- !.

eval(E, Val) :-
    functor(E, Name, 2),
    member(Name, [+, *]),
    !,
    eval_arith_binop(E, Val).

eval((EA ++ EB), Val) :-
    !,
    eval(EA, VA),
    eval(EB, VB),
    append(VA, VB, Val).

eval((Pat = E), Val) :-
    !,
    eval(E, Val),
    Pat = Val.

eval((EA; EB), Val) :-
    !,
    eval(EA, _),
    eval(EB, Val).

eval(Exp, Val) :-
    current_arithmetic_function(Exp),
    !,
    Val is Exp.

eval(Exp, Val) :-
    functor(Exp, _, _),
    !,
    call(Exp, Val).
