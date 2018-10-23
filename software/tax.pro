/*
This is not a complete implementation.
Don't use this for computing your actual taxes.
*/

% PPh (Pajak Penghasilan)

:- module(tax, [

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

    % Calculations.

    , taxation_income_tax/2

]).
:- use_module('date.pro').

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
PPh pasal 21.

Income is gross income in the tax year.

taxation_income_tax(Subject, Income, Tax).
*/

subject_missing_npwp_multiplier(S, M) :-
    subject_has_npwp(S, true), M = 1.0
;   subject_has_npwp(S, false), M = 1.2.

taxation_income_tax(Taxation, Tax) :-
    taxation_subject(Taxation, Subject),
    subject_missing_npwp_multiplier(Subject, Missing_npwp_multiplier),
    income_tax_has_npwp(Taxation, Sub),
    Tax is Missing_npwp_multiplier * Sub.

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

% Penghasilan Tidak Kena Pajak

% ptkp(Taxation, Amount).
ptkp_amount(Taxation, Amount) :-
    taxation_year(Taxation, Year),
    ptkp_single_amount(Year, Amount).

% ptkp_single_amount(Year, Amount).
ptkp_single_amount(2018, 54 000 000).
