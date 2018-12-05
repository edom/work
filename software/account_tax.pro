:- module(account_tax, [

    % Structures.

    taxation/1
    , taxation_subject/2
    , taxation_year/2

    , subject/1
    , subject_has_npwp/2
    , subject_gross_income/2

    , income/1
    , income_year/2
    , income_amount/2

    , bracket/1

    % Calculations.

    , taxation_report/2
    , income_brackets/2
    , report_format/1

]).
:- use_module('date.pro').
/** <module> Indonesian tax accounting?

This is not a complete implementation.
Don't use this for computing your actual taxes.
*/

taxation(A) :- functor(A, taxation, 2).
taxation_subject(A, B) :- taxation(A), arg(1, A, B).
taxation_year(A, B) :- taxation(A), arg(2, A, B).

subject(A) :- functor(A, subject, 2).
subject_has_npwp(A, B) :- subject(A), arg(1, A, B).
subject_gross_income(A, B) :- subject(A), arg(2, A, B).

income(A) :- functor(A, income, 2).
income_year(A, B) :- income(A), arg(1, A, B).
income_amount(A, B) :- income(A), arg(2, A, B).

/*
- A bracket is a term bracket(Min, Max, Rate, Amount, Tax).
  - Min is inclusive.
  - Max is exclusive.
  - Amount is the amount of Income that falls in the bracket.
    Amount may be zero.
*/
bracket(A) :- functor(A, bracket, 5).
bracket_min(A, B) :- bracket(A), arg(1, A, B).
bracket_max(A, B) :- bracket(A), arg(2, A, B).
bracket_rate(A, B) :- bracket(A), arg(3, A, B).
bracket_amount(A, B) :- bracket(A), arg(4, A, B).
bracket_tax(A, B) :- bracket(A), arg(5, A, B).

subject_missing_npwp_multiplier(S, M) :-
    subject_has_npwp(S, true), M = 1.0
;   subject_has_npwp(S, false), M = 1.2.

report(A) :- functor(A, report, 2).
report_pph21_brackets(A, B) :- report(A), arg(1, A, B).
report_pph21_amount(A, B) :- report(A), arg(2, A, B).

/*
PPh pasal 21.

Income is gross income in the tax year.

taxation_report(Taxation, Report).
*/

taxation_report(Taxation, Report) :-
    taxation_subject(Taxation, Subject),
    subject_missing_npwp_multiplier(Subject, Missing_npwp_multiplier),
    income_tax_has_npwp(Taxation, Sub),
    Tax is Missing_npwp_multiplier * Sub,
    subject_gross_income(Subject, Income),
    income_brackets(Income, Brackets),
    report_pph21_brackets(Report, Brackets),
    report_pph21_amount(Report, Tax).

report_format(Report) :-
    report_pph21_brackets(Report, Brackets),
    forall(member(Bracket, Brackets), bracket_format(Bracket)).

bracket_format(Bracket) :-
    bracket_min(Bracket, Min),
    bracket_max(Bracket, Max),
    bracket_rate(Bracket, Rate),
    bracket_amount(Bracket, Amount),
    bracket_tax(Bracket, Tax),
    bracket_max_format(Max, S_max),
    Tax_rounded is round(Tax),
    format('~t~D -- ~w~30+~t~3f~10+~t~D~20+~t~D~20+~n', [Min, S_max, Rate, Amount, Tax_rounded]).

bracket_max_format(inf, "inf").
bracket_max_format(D, S) :- integer(D), format(string(S), '~D', D).

income_tax_has_npwp(Taxation, Tax) :-
    taxation_subject(Taxation, Subject),
    subject_has_npwp(Subject, true),
    taxation_year(Taxation, Year),
    subject_gross_income(Subject, Income),
    income_year(Income, Year),
    income_amount(Income, Gross),
    ptkp_amount(Taxation, Ptkp),
    Pkp is Gross - Ptkp,
    compute_income_tax(Pkp, Tax).

compute_income_tax(Income, Tax) :-
    Income =< 0,
        Tax is 0

;   0 < Income,
    Income =< 50 000 000,
        Tax is 0.05 * Income

;   50 000 000 < Income,
    Income =< 250 000 000,
        compute_income_tax(50 000 000, Tax_0),
        Tax is Tax_0 + 0.15 * (Income - 50 000 000)

;   250 000 000 < Income,
    Income =< 500 000 000,
        compute_income_tax(250 000 000, Tax_0),
        Tax is Tax_0 + 0.25 * (Income - 250 000 000)

;   500 000 000 < Income,
        compute_income_tax(500 000 000, Tax_0),
        Tax is Tax_0 + 0.30 * (Income - 500 000 000).

/*
Brackets is a list of brackets.

The amounts in all brackets should add up to the input Income.
*/
income_brackets(Income, [
    bracket(Min0, Max0, Rat0, Amt0, Tax0),
    bracket(Min1, Max1, Rat1, Amt1, Tax1),
    bracket(Min2, Max2, Rat2, Amt2, Tax2),
    bracket(Min3, Max3, Rat3, Amt3, Tax3)
]) :-
    income_year(Income, 2018),
    income_amount(Income, Inc0),

    Min0 = 500 000 000,
    Max0 = inf,
    Rat0 = 0.30,
    Amt0 is max(0, Inc0 - Min0),
    Tax0 is Rat0 * Amt0,

    Inc1 = Inc0 - Amt0,
    Min1 = 250 000 000,
    Max1 = Min0,
    Rat1 = 0.25,
    Amt1 is max(0, Inc1 - Min1),
    Tax1 is Rat1 * Amt1,

    Inc2 = Inc1 - Amt1,
    Min2 = 50 000 000,
    Max2 = Min1,
    Rat2 = 0.15,
    Amt2 is max(0, Inc2 - Min2),
    Tax2 is Rat2 * Amt2,

    Inc3 = Inc2 - Amt2,
    Min3 = 0,
    Max3 = Min2,
    Rat3 = 0.05,
    Amt3 is max(0, Inc3 - Min3),
    Tax3 is Rat3 * Amt3.

% Penghasilan Tidak Kena Pajak

% ptkp(Taxation, Amount).
ptkp_amount(Taxation, Amount) :-
    taxation_year(Taxation, Year),
    ptkp_single_amount(Year, Amount).

% ptkp_single_amount(Year, Amount).
ptkp_single_amount(2018, 54 000 000).
