:- module(interval, [
    interval/1
    , interval_empty/1
    , interval_non_empty/1
    , interval_intersection/3
]).

:- use_module(library(clpfd)).

/*
An interval looks like A-B.
The end is exclusive.
*/
interval(_-_).

interval_empty(A-B) :- A #>= B.

interval_non_empty(A-B) :- A #< B.

% An interval A-B is empty iff A >= B.
interval_intersection(A0-A1, B0-B1, C0-C1) :-
    C0 #= max(A0, B0),
    C1 #= min(A1, B1).
