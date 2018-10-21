/*
See https://docs.oracle.com/javase/specs/jls/se11/html/index.html

This is not a complete implementation.
*/

:- module(java_syntax, [
]).

:- use_module(library(clpfd)).

% Location.

location_begin(location(1, 1)).
location_line_column(location(L, C), L, C).

% location_right(A, N, B) means that B is N characters after A.

location_right(location(L, C), N, location(L, CC)) :- CC #= C + N.

location_right(A, B) :- location_right(A, 1, B).

location_newline(location(L, _), location(LL, 1)) :- LL #= L + 1.

/*
A concrete syntax tree node is a list of properties.

Problem: How do we add location, error-reporting, and recovery?
How do we add location information to a grammar without dirtying the grammar?

Naively adding location dirties DCG with Loc parameters and loc_right phrases.
For example:

    a --> b0, b1, b2.
    b0 --> "b".

becomes the dirty:

    a(Loc_A) --> b0(Loc_0, Loc_1), b1(Loc_1, Loc_2), b2(Loc_2, Loc_3), Loc_3 = Loc_A.
    b0(Loc_0, Loc_1) :- "b", {loc_right(Loc_0, Loc_1)}.

rules([
    number ::= at_least_one(digit)
])
*/

/*
Transform

    head(H1, ..., Hn) ==> Body

to

    head(Node, H1, ..., Hn) -->
        input(Input),
        Body,
        remaining(Remaining),
        append(Codes, Remaining, Input),
        Node = head().
*/

:- op(1200, xfx, '==>').

% TODO
term_expansion((Head ==> Body), (Dcg_head --> Dcg_body)) :- true
    , Head =.. [Functor | Params]
    , Dcg_head =.. [Functor, Codes | Params]
    , Dcg_body = (input(Input), Body, remaining(Remaining), {lists:append(Codes, Remaining, Input)})
    .

match(Rule, Match, A, B) :- true
    , Goal =.. [Rule, A, B]
    , call(Goal)
    , append(Match, B, A)
    .

% Use these to get DCG generated parameters.
input(A,A,_).
remaining(B,_,B).

% A node is a compound Type(Source) where Source is list of codes.
% A lexing error is represented with error(Message, Source).

decimal_numeral --> digit.

digit --> [Code], {code_type(Code, digit)}.

number_ -->
    digit, number_
;   digit
.

operator -->
    "+"
.

digit([Code | U], [Code], U) :- code_type(Code, digit).

number(U0, P0, U1) :- digit(U0, P0, U1).
number(U0, P2, U2) :- digit(U0, P0, U1), number(U1, P1, U2), append(P0, P1, P2).

operator([Code | U], [Code], U) :- string_codes("+-", Codes), member(Code, Codes).

expression(U0, P0, U1) :- number(U0, P0, U1).
expression(U0, P3, U3) :- true
    , operator(U1, P1, U2)
    , expression(U0, P0, U1)
    , expression(U2, P2, U3)
    , append([P0, P1, P2], P3)
    .

white_space --> [Code], {code_type(Code, space)}.

line_terminator --> [Code], member(Code, [10, 13]).

digit([C]) :- code_type(C, digit).

number_([H]) :- digit([H]).
number_([H|T]) :- digit([H]), number_(T).

operator([0'+]).

expression(number(E), E) :- number_(E).
expression(plus(EA,EC), E) :- true
    , operator(B)
    , append([A,B,C], E)
    , expression(EA,A)
    , expression(EC,C)
    .
