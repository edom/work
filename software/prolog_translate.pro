:- module(prolog_translate, [
    clause_mgh/2,
    clausehead_mgu/3,
    mghclause_disjunct/3,
    mghclauses_disjunct/2,
    pred_dnfclause/2,
    pred_mghclause/2,
    pred_mghclauses/2,
    foo/1
]).
:- use_module(library(error)).
:- use_module('./language_prolog.pro').
/** <module> Prolog program analysis?

Note:
Do not use.
Most of this file has been superseded by the file language_prolog.pro.

In this document:
    - "To explicate" means "to make explicit".
    - A "predicate indicator" is a Name/Arity where Name is an atom and Arity is a nonnegative integer.
    - DNF stands for "disjunctive normal form".
    - "mgh" stands for "most general head".

The transformation steps:
    - Normalize to DNF: convenience predicate pred_dnfclause/2.
        - Enumerate the clauses of a predicate: builtin clause/2 and findall/3.
        - Maximally generalize each clause head: clause_mgh/2.
        - Collect explicated clauses: pred_mghclauses/2, pred_mghclause/2.
        - Join the explicated clauses into one DNF clause: mghclauses_disjunct/2.
    - Analyze determinacy: det (always true without choice points), semidet (true at most once), nondet.
        - Translate a det predicate to a function.
    - Translate to abstract procedural language.
        - Translate unification.
            - Each variable _refers_ to a _location_ (an _lvalue_ in C parlance, a _place_ in Lisp parlance).
                - Unifying two variables creates (allocates) a new location and sets those variables to point to this new shared location.
                - Unifying a variable and a value (ground term) puts the value in the location.
                - Unifying two values compares the terms.
                - Failure destroys (frees/deallocates) all locations allocated while the code was in the frame.
                - Destroying a frame destroys all location allocated in that frame and restores all variables set in that frame.
        - Deduplicate atoms to integers.
        Emit atom table.
        Assume that the program does not create atoms.
        The atom table is constant.
        - Main (entry point) is a query.
        - Are there alternatives to WAM? https://en.wikipedia.org/wiki/Warren_Abstract_Machine
    - Predicate argument groundness analysis:
        - If argument A is always ground on predicate entry,
        then A can be replaced with a unidirectional C input argument.
        - If the predicate always grounds the argument A on exit,
        then A can be replaced with a unidirectional C output argument.
        (pointer to caller-allocated memory).
    - A nondet Prolog predicate is essentially a C iterator.

Representing a term:
    - An atom is represented by a 16-bit unsigned integer that is a key in the atom table.
    Atom comparison is integer comparison.
    The atom table is constant.
    The program cannot create new atoms at runtime; use strings for that.
    - A compound term is represented by (Name,Arity,Arg1,...,ArgN) where Name is an atom, Arity is a 16-bit unsigned integer.
    - A string is represented by (Limit,Length,Bytes) where Limit takes 4 bytes, Length takes 4 bytes, and Bytes is the contents of the string.

Creating a choice point is similar to installing an exception handler.
Failing (backtracking) is similar to throwing an exception and unwinding the stack.

A predicate with N parameters may compile to at most 2^N routines.
If we know that a variable is ground when we enter a predicate,
we can optimize it.

```
% This translates to two routines: p_g, p_u
p(0).
p(1).
p(A) :- A >= 5.

% This translates to four routines: b_gg, b_gu, b_ug, b_uu
b(1,0).
b(2,1).
b(A,B) :- A =< B.

% proc1 calls p_g.
proc1 :- p(0), write(hi).

% proc2 calls p_u.
% If proc2 gets inlined, it may call p_g, depending on context.
proc2(A) :- p(A).
```

We want frames (delimited continuations) (shift+reset) for cuts.

Prolog should use three-valued logic with three truth values: true, unknown, and false.
"A \= B" should be unknown, not false.
Three-valued logic simplifies constraint logic programming?
Prolog should use Kleene's "strong logic of indeterminacy" or Priest's "logic of paradoxes"?
    - https://en.wikipedia.org/wiki/Three-valued_logic

Supporting built-ins:
    - =../2 converts a functor to a representation that is less efficient but more manipulable.
    - portray_clause/2 for pretty-printing

Problems:
    - This does not work with modules.

Literature:
    - "?-Prolog compiles a single Prolog source file into C"
    http://www.call-with-current-continuation.org/prolog/README.html
    - 1992
    "Use of Prolog for developing a new programming language"
    "This paper describes how Prolog was used for the development of a new concurrent realtime symbolic programming language called Erlang."
    https://pdfs.semanticscholar.org/57d3/1ca47fa9688089b9b7e7c19c199aa03aff1e.pdf
*/

foo(a).
foo(b).
foo(A) :- integer(A), !.

/** pred_dnfclause(++Indicator, -Dnf)

"Dnf is a disjunctive normal form clause that is equivalent to the predicate described by Indicator."

Indicator is Name/Arity.
*/
pred_dnfclause(Ind, Dnf) :-
    pred_mghclauses(Ind, Ecls),
    mghclauses_disjunct(Ecls, Dnf).

/**
pred_mghclause(++NameArity, -Clause).
pred_mghclauses(++NameArity, -Clauses).

Clause is =|Head :- Body|=.

Clauses is a list of Clause.
*/
pred_mghclause(Ind, Clause) :-
    Ind = Name/Arity,
    pred_must_exist(Ind),
    functor(Head, Name, Arity),
    clause(Head, Body),
    clause_mgh((Head :- Body), Clause).

pred_must_exist(Ind) :-
    (pred_exists(Ind) -> true
    ; throw(error(unknown_predicate(Ind), _))
    ).

pred_exists(Name/Arity) :-
    functor(Head, Name, Arity),
    predicate_property(Head, visible).

prolog:error_message(unknown_predicate(Ind)) -->
    ['predicate ~p does not exist, or contains typo, or is not loaded, or is autoloadable but not loaded'-[Ind]].

pred_mghclauses(Ind, Cls) :-
    findall(Cl, pred_mghclause(Ind, Cl), Cls).

mghclause_disjunct((Head :- Body1), (Head :- Body2), (Head :- (Body1 ; Body2))).

mghclauses_disjunct([C], C) :- !.
mghclauses_disjunct([C|Cs], D) :-
    mghclauses_disjunct(Cs,Ds),
    mghclause_disjunct(C,Ds,D).
