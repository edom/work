:- module(date, [
    date_compare/3
    , date_before/2
    , date_equal/2
    , date_after/2
]).

:- use_module(library(clpfd)).

/*
This assumes that the date is valid.

If you don't want to this, you can also compare dates with the standard order of terms @</2.
*/
date_before(date(Y0, M0, D0), date(Y1, M1, D1)) :-
    Y0 #< Y1
;   Y0 #=< Y1, M0 #< M1
;   Y0 #=< Y1, M0 #=< M1, D0 #< D1.

date_after(A, B) :- date_before(B, A).

date_equal(date(Y, M, D), date(Y, M, D)).

% date_compare(Operator, Date_0, Date_1).
date_compare('<', A, B) :- date_before(A, B).
date_compare('=<', A, B) :- date_before(A, B); date_equal(A, B).
date_compare('>', A, B) :- date_after(A, B).
date_compare('>=', A, B) :- date_after(A, B); date_equal(A, B).
date_compare('=', A, B) :- date_equal(A, B).
