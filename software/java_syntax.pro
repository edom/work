/*
See https://docs.oracle.com/javase/specs/jls/se11/html/index.html

This is not a complete implementation.
*/

:- module(java_syntax, [
    expression//1
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

number([First|Rest]) --> digit(First), (number(Rest) ; {Rest = []}).

digit(Code) --> [Code], {code_type(Code, digit)}.

space(Code) --> [Code], {code_type(Code, space)}.

expression(plus(M, N)) --> expression_number(M), "+", expression_number(N).

expression_number(Number) --> number(Codes), {number_codes(Number, Codes)}.
