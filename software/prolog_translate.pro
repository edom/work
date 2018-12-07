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
/** <module> Prolog program analysis?

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

/** clause_mgh(+Clause, -NClause)

"NClause is Clause with most general head."

If Clause looks like this:
```
p(a,b,c) :- Body
```
then NClause looks like this:
```
p(A,B,C) :- A=a, B=b, C=c, Body
```
*/
clause_mgh((Head :- Body), (NHead :- NBody)) :- !,
    clausehead_mgu(Head, NHead, Unifs),
    conjunction_conjuncts(Body, CBody),
    append(Unifs, CBody, CNBody),
    conjunction_conjuncts(NBody, CNBody).

/** clausehead_mgu(+Head, -NormHead, -Unifiers)

Relate the clause head and a same-named functor whose all arguments are variables.

Relate the clause head, the most general form of the clause head, and what needs to be prepended to the clause body.

MGU stands for "most general unifier".
This seems to be a standard terminology in logic?

Head is a functor.

NormHead is Head but with each nonvar argument replaced with a fresh variable.

Unifiers is a list of phrases that would unify Head and NormHead.
Each element has the shape A=B where A is a variable and B is a nonvar.
See example.

Example:

```
?- clausehead_mgu(p(A,b,C,d), N, U).
N = p(A, _6642, C, _6654),
U =  [_6642=b, _6654=d].
```

This is similar to the builtin unifiable/3.
The differences:
    - unifiable/3 handles cyclic terms. This does not.
    - unifiable/3 returns the Unifiers in reverse order. This returns the Unifiers in forward order.

See also:
    - Skolem normal form is "prenex normal form with only universal first-order quantifiers".
    https://en.wikipedia.org/wiki/Skolem_normal_form
    - most general unifier
*/
clausehead_mgu(Head, Norm, Unifs) :-
    Head =.. [Name | Args],
    length(Args, Arity),
    length(Vars, Arity),
    Norm =.. [Name | Vars],
    args_vars_unifs_(Args, Vars, Unifs).

args_vars_unifs_([A|As], [V|Vs], Us) :- var(A), !, V=A, args_vars_unifs_(As,Vs,Us).
args_vars_unifs_([A|As], [V|Vs], [V=A | Us]) :- nonvar(A), !, args_vars_unifs_(As,Vs,Us).
args_vars_unifs_([], [], []).

/** conjunction_conjuncts(?Conjunction, ?Conjuncts)

"Conjunction is a right-associative conjunction of Conjuncts."

At least one argument must be bound.

Conjunction is a term like =|(a,b,c)|=.
Note that the comma operator associates to the right:
```
?- write_canonical((a,b,c)).
','(a,','(b,c))
```

Conjuncts is a list like =|[a,b,c]|=.
*/
conjunction_conjuncts(A, [A]) :- A \= true, !.
conjunction_conjuncts(true, []) :- !.
conjunction_conjuncts((A,B), [A|P]) :- !, conjunction_conjuncts(B,P).

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
