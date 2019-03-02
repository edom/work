:- module(language_prolog, [
    kb_querynaive/2,
    kb_query/2,
    prolog_transitive_closure/3,
    kb_query_transitive_closure/4,
    kb_pred_horndnf/3,
    kb_pred_mghclause/3,
    kb_pred_mghclauses/3,
    horn_mgh/2,
    hornhead_mgu/3,
    mghhorn_disjunct/3,
    mghhorns_disjunct/2,
    functor_addargs/3,
    op(1,fx,'#')
]).
:- reexport('./language_prolog_callgraph.pro').
:- reexport('./language_prolog_clause.pro').
:- reexport('./language_prolog_kb.pro').
:- reexport('./language_prolog_transput.pro').
:- use_module(library(nb_set)).

/** <module> Prolog meta-interpreter

See also the file prolog_translate.pro.

There are three kinds of clauses:
    - Horn clause =|A :- B|=
    - disjunctive clause =|A ; B|=
    - conjunctive clause =|A , B|=

Reading knowledge bases:
    - readfile_kb/2 reads a knowledge base from a file.
    - readstream_kb/2 reads a knowledge base from a stream.
    - readstring_kb/2 reads a knowledge base from a string.

Querying knowledge bases:
    - kb_querynaive/2 interprets.
    - kb_query/2 interprets with memoization.
    - kb_rule/3 looks up.
    - kb_head_body/3 looks up.
    - kb_pred_horndnf/3 normalizes.

Computing transitive closure:
    - prolog_transitive_closure/3
    - kb_query_transitive_closure/4

Computing knowledge base call graph:
    - kb_callgraphtc/2 computes the transitive closure of the call graph using library(ugraphs).
    - kb_calledges/3 computes the edge list of the call graph.
    - kb_caller_callee/3 finds direct calls.

Collecting the outermost phrases of a clause:
    - clause_disjuncts/2 collects the outermost conjuncts.
    - clause_conjuncts/2 collects the outermost disjuncts.
    - conjuncts_clause/2 is the inverse of clause_conjuncts/2.
    - disjuncts_clause/2 is the inverse of clause_disjuncts/2.
*/

:- op(1,fx,'#').

/**
kb_querynaive(+Rules,+Query).
kb_query(+Rules,+Query).

Answer the Query (prove the goal) according to Rules.

Answering a query = proving a goal.

There is no cut, but there is =|#once(A)|=.

The predicate kb_query/2 is kb_querynaive/2 with memoization.
Note that a memoized predicate produces only the first result produced by the unmemoized predicate.

Memoization must be explicitly requested by having a =|:-memoize(Name/Arity)|= fact in the Rules.
*/
kb_querynaive(_,A) :- invalid_query(A).
kb_querynaive(K,#once(A)) :- !, once(kb_querynaive(K,A)).
kb_querynaive(_,#A) :- !, primitive(A).
kb_querynaive(_,(A=B)) :- !, A=B.
kb_querynaive(K,(A,B)) :- !, kb_querynaive(K,A), kb_querynaive(K,B).
kb_querynaive(K,(A;_)) :- kb_querynaive(K,A).
kb_querynaive(K,(_;A)) :- !, kb_querynaive(K,A).
kb_querynaive(K,H) :- kb_rule(K,H).
kb_querynaive(K,H) :- kb_rule(K,(H:-B)), kb_querynaive(K,B).

    invalid_query(A) :- var(A), !, type_error(nonvar,A).
    invalid_query(A) :- string(A), !, type_error(query,A).
    invalid_query(A) :- number(A), !, type_error(query,A).
    invalid_query((:-A)) :- !, domain_error(query,(:-A)).
    invalid_query((H:-B)) :- !, domain_error(query,(H:-B)).

    primitive(append(A,B)) :- !, append(A,B).
    primitive(append(A,B,C)) :- !, append(A,B,C).
    primitive(true) :- !.
    primitive(A) :- !, domain_error(primitive,A).

kb_query(K,A) :-
    findall(F,(member((:-memoize(F)),K)),Whitelist),
    memofromset([],M),
    memofromset([],N),
    kb_query(Whitelist,K,M,N,A).

    % kb_query(+Whitelist, +Rules, !TrueGrounds, !FalseGrounds, +Goal)
    kb_query(_,_,_,_,A) :- invalid_query(A).
    kb_query(W,K,M,N,#once(A)) :- !, once(kb_query(W,K,M,N,A)).
    kb_query(_,_,_,_,#A) :- !, primitive(A).
    kb_query(_,_,_,_,(A=B)) :- !, A=B.
    kb_query(W,K,M,N,(A,B)) :- !, kb_query(W,K,M,N,A), kb_query(W,K,M,N,B).
    kb_query(W,K,M,N,(A;_)) :- kb_query(W,K,M,N,A).
    kb_query(W,K,M,N,(_;A)) :- !, kb_query(W,K,M,N,A).
    kb_query(_,_,M,_,H) :- memo_member(M,H), !.
    kb_query(_,_,_,N,H) :- memo_member(N,H), !, false.
    kb_query(W,K,M,_,H) :- kb_rule(K,H), nb_memoize(W,M,H).
    kb_query(W,K,M,N,H) :- kb_rule(K,(H:-B)), kb_query(W,K,M,N,B), nb_memoize(W,M,H).
    kb_query(W,_,_,N,H) :- nb_memoize(W,N,H), !, false.

        nb_memoize(W,M,F) :- shouldmemoize(W,M,F), !, domemoize(M,F).
        nb_memoize(_,_,_) :- !.

/*
            % naive list

            memofromset(Set,m(Set)).

            memo_member(m([A|_]),A).
            memo_member(m([_|B]),A) :- memo_member(m(B),A).

            domemoize(M,F) :- arg(1,M,N), ord_union(N,[F],P), nb_setarg(1,M,P).
*/

            memofromset(L,M) :- empty_nb_set(M), memofromset0(L,M).

                memofromset0([],_) :- !.
                memofromset0([A|B],M) :- !, add_nb_set(A,M), memofromset0(B,M).

            % add_nb_set(A,M) does not work if A is not ground.
            % If so, what's the point of using nb_set?
            % memo_member(M,A) :- add_nb_set(A,M,false).
            memo_member(M,A) :- gen_nb_set(M,B), B=A.

            domemoize(M,F) :- add_nb_set(F,M).

        shouldmemoize(_,_,#_) :- !, false.
        shouldmemoize(_,_,(_=_)) :- !, false.
        shouldmemoize(_,_,A) :- \+ ground(A), !, false.
        shouldmemoize(W,_,A) :- whitelisted(W,A).

            whitelisted(W,Exp) :- functor(Exp,Name,Arity), member(Name/Arity,W), !.

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

functor_addargs(F0,Args,F1) :- F0=..F, append(F,Args,G), F1=..G.

/**
prolog_transitive_closure(+Call, ?X, ?Y).

Call must be a functor that will accept two more parameters.
Call should not contain any variables; otherwise the result may be too confusing.
*/
:- meta_predicate(prolog_transitive_closure(2,?,?)).
prolog_transitive_closure(Call,A,B) :- ptc([],Call,A,B).

    % Tests look OK, but how do we prove that this works?
    ptc(Visited,Call,A,B) :- call(Call,A,B), \+ member(B,Visited).
    ptc(Visited,Call,A,B) :- call(Call,A,C), \+ member(C,Visited), ptc([C|Visited],Call,C,B).

kb_query_transitive_closure(K,Call,A,B) :- ktc(K,[],Call,A,B).

    ktc(K,Visited,Call,A,B) :- kb_callargs(K,Call,[A,B]), \+ member(B,Visited).
    ktc(K,Visited,Call,A,B) :- kb_callargs(K,Call,[A,C]), \+ member(C,Visited), ktc(K,[C|Visited],Call,C,B).

        kb_callargs(K,F,Args) :- functor_addargs(F,Args,C), kb_querynaive(K,C).
