:- module(language_prolog_clause, [
    clause_conjuncts/2,
    clause_disjuncts/2,
    conjuncts_clause/2,
    disjuncts_clause/2
]).
/** <module> Prolog clause manipulation; part of language_prolog.pro
*/

/**
clause_conjuncts(+Clause, --List).
clause_disjuncts(+Clause, --List).
conjuncts_clause(+List, --Clause).
disjuncts_clause(+List, --Clause).

List must not be empty.

These predicates only work in one direction.
The output argument must be an unbound variable.
Violation of this assumption results in wrong result or non-termination.
*/
clause_conjuncts((A,B),C) :- !, clause_conjuncts(A,AA), clause_conjuncts(B,BB), append(AA,BB,C).
clause_conjuncts(A,[A]) :- A \= (_,_).

conjuncts_clause([J], J) :- !.
conjuncts_clause([JA|JB], (JA,CB)) :- !, conjuncts_clause(JB,CB).

clause_disjuncts((A;B),C) :- !, clause_disjuncts(A,AA), clause_disjuncts(B,BB), append(AA,BB,C).
clause_disjuncts(A,[A]) :- A \= (_;_).

disjuncts_clause([J], J) :- !.
disjuncts_clause([JA|JB], (JA;CB)) :- disjuncts_clause(JB,CB).
