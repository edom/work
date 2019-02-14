:- module(language_prolog, [
    kb_query/2,
    kb_head_body/3,
    kb_pred_horndnf/3,
    kb_pred_mghclause/3,
    kb_pred_mghclauses/3,
    horn_mgh/2,
    hornhead_mgu/3,
    mghhorn_disjunct/3,
    mghhorns_disjunct/2,
    op(1,fx,'#')
]).
/** <module> Prolog meta-interpreter

See also the file prolog_translate.pro.

Knowledge-base functions:
    - kb_query/2 interprets.
    - kb_head_body/3 looks up.
    - kb_pred_horndnf/3 normalizes.
*/

:- op(1,fx,'#').

/** kb_query(Kb,Query)

Answer the Query (prove the goal) using Kb.

Answering a query = proving a goal.

There is no cut, but there is =|#once(A)|=.
*/
kb_query(K,(A,B)) :- !, kb_query(K,A), kb_query(K,B).
kb_query(K,(A;_)) :- kb_query(K,A).
kb_query(K,(_;A)) :- !, kb_query(K,A).
kb_query(_,#append(A,B,C)) :- !, append(A,B,C).
kb_query(K,#once(A)) :- !, once(kb_query(K,A)).
kb_query(K,H) :- kb_head_body(K,H,B), kb_query(K,B).

    /** clause_conjuncts(+Clause, -List)
    */
    clause_conjuncts(C,Js) :- findall(J, clause_conjunct(C,J), Js).

        clause_conjunct((A,_),A).
        clause_conjunct((_,A),A).
        clause_conjunct(A,B) :- A \= (_,_), A = B.

    /** conjuncts_clause(+List, -Clause)
    */
    conjuncts_clause([J], J).
    conjuncts_clause([JA|JB], (JA,CB)) :- conjuncts_clause(JB,CB).

/** kb_head_body(+List, ?Head, ?Body)

Match (Head :- Body) in List; or match Head in List and set Body = true.

This is similar to clause/2, but this looks only in the List instead of all loaded definitions.

This cannot be done by member/2 because this requires copy_term/2.
*/
kb_head_body([C|_],H,B) :- copy_term(C,(H:-B)).
kb_head_body([C|_],H,true) :- copy_term(C,H).
kb_head_body([_|R],H,B) :- kb_head_body(R,H,B).

/** horn_mgh(+Clause, -NClause)

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
horn_mgh((Head :- Body), (NHead :- NBody)) :- !,
    hornhead_mgu(Head, NHead, Unifs),
    conjunction_conjuncts(Body, CBody),
    append(Unifs, CBody, CNBody),
    conjunction_conjuncts(NBody, CNBody).

/** hornhead_mgu(+Head, -NormHead, -Unifiers)

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
?- hornhead_mgu(p(A,b,C,d), N, U).
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
    hornhead_mgu(Head, Norm, Unifs) :-
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

/** kb_pred_horndnf(++Indicator, -Dnf)

"Dnf is a Horn clause whose head is in most general form and whose body is in disjunctive normal form.
The Horn clause is equivalent to the predicate described by Indicator."

Indicator is Name/Arity.
*/
kb_pred_horndnf(K, Ind, Dnf) :-
    kb_pred_mghclauses(K, Ind, Ecls),
    mghhorns_disjunct(Ecls, Dnf).

    kb_pred_mghclauses(K, Ind, Cls) :-
        findall(Cl, kb_pred_mghclause(K, Ind, Cl), Cls).

/**
kb_pred_mghclause(++NameArity, -Clause).
kb_pred_mghclauses(++NameArity, -Clauses).

Clause is =|Head :- Body|=.

Clauses is a list of Clause.
*/
        kb_pred_mghclause(K, Ind, Clause) :-
            Ind = Name/Arity,
            kb_pred_must_exist(K, Ind),
            functor(Head, Name, Arity),
            kb_head_body(K, Head, Body),
            horn_mgh((Head :- Body), Clause).

            kb_pred_must_exist(K, Ind) :-
                (kb_pred_exists(K, Ind) -> true
                ; throw(error(unknown_predicate(Ind), _))
                ).

                kb_pred_exists(K, Name/Arity) :-
                    functor(Head, Name, Arity),
                    once(kb_head_body(K, Head, _)).

                prolog:error_message(unknown_predicate(Ind)) -->
                    ['predicate ~p does not exist'-[Ind]].

    mghhorns_disjunct([C], C) :- !.
    mghhorns_disjunct([C|Cs], D) :-
        mghhorns_disjunct(Cs,Ds),
        mghhorn_disjunct(C,Ds,D).

        mghhorn_disjunct((Head :- Body1), (Head :- Body2), (Head :- (Body1 ; Body2))).
