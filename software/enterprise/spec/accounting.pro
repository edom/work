:- include("include/accounting_type.pro").
:- include("include/accounting_behavior.pro").
:- include("include/accounting_webapp.pro").

% -------------------- globalization

term_locale_string(hello, eng, "Hello.").

% -------------------- code generation parameters

base_package_name("com.spacetimecat.java").
maven_coordinates(GroupId, "accounting", "0.0.0") :- base_package_name(GroupId).
output_dir("out").

:- if(getenv('DRY_RUN', _)).
dry_run(true).
:- else.
dry_run(false).
:- endif.

% ------- dreams

/** inline(+Pattern,+Term,-Replacement) is nondet.

This can be used to enumerate possible partial deduction with respect to the currently loaded predicates.

Constraint: =call(Pattern)= should only succeed a finite number of times.

Example:

==
p(A) :- q(A), r(A).

q(A) :- r(B), A is B + 100.

r(0).
r(1).

?- inline(q(_), (p(A) :- q(A),r(A)), Z), write(Z), nl, fail.

% produces:

p(100):-true,r(100)
p(101):-true,r(101)
==

How this may be useful:

    1. Write a naive interpreter for the Prolog terms representing the abstract syntax of a language.
    2. Use partial deduction to derive a Prolog program equivalent to the interpreter but much faster.

Problem: This inline/3 does not work for recursive predicates:

==
interpret(A,Z) :- number(A), !, Z = A.
interpret(A+B,Z) :- Z is A+B.
interpret(A*B,Z) :- Z is A*B.

?- inline(interpret(_), (read(A), interpret(A,B), write(B), nl), Program).
==

Maybe we shouldn't use call/1.

*/
inline(Pattern, (A:-B), Z) :- !, Z = (A:-C), inline(Pattern, B, C).
inline(Pattern, (A,B), (Y,Z)) :- !, inline(Pattern,A,Y), inline(Pattern,B,Z).
inline(Pattern, (A;B), (Y;Z)) :- !, inline(Pattern,A,Y), inline(Pattern,B,Z).
inline(Pattern, (A->B), (Y->Z)) :- !, inline(Pattern,A,Y), inline(Pattern,B,Z).
inline(Pattern, Term, true) :- unifiable(Pattern,Term,_), copy_term(Pattern,Term), call(Term).
inline(_, Term, Term).

% ------- Dream: Translate Prolog/Datalog to SQL.

:- dynamic employee/1,
           employee_name/2,
           employee_join_date/2.

% Dream: Translate employee/1 and its properties to database query.
dream_query((
    employee_old_timer(E) :-
        now(Now),
        employee_join_date(E,J),
        subtract_date(Now,J,Duration),
        Duration >= duration(10,year)
)).

dream_query((
    engineer(E) :-
        employee_department(E,D),
        department_name(D,"Engineering")
)).
