:- module(prolog0, [
    interpret/2,
    subst/4,
    vars/2,
    op(1,fx,$),
    op(50,yfx,ap),
    op(1150,xfy,=>),
    example_0/0
]).
/** <module> Prolog with anonymous predicates
*/
:- op(1,fx,$). % substitution
:- op(50,yfx,ap). % x ap y ap z = (x ap y) ap z
:- op(1150,xfy,=>). % lambda abstraction

/** interpret(+Context, +Goal)

Prove Goal in Context.

*/
interpret(_,true) :- !.
interpret(C,dump_context) :- !, print_term(C,[]), nl.
interpret(C,once(G)) :- !, once(interpret(C,G)).
interpret(C,(A,B)) :- !, interpret(C,A), interpret(C,B).
interpret(C,with(Binds, Body)) :- !, append(Binds,C,C0), interpret(C0, Body).
interpret(C,with_import(Import, Body)) :- !, import(Import,Binds), interpret(C, with(Binds,Body)).
% BEGIN should we have these?
interpret(C,current_context(C)) :- !.
interpret(C,clause(H,B)) :- !, match(C,H,B).
interpret(_,prolog_call(G)) :- !, call(G).
interpret(_,pretty(A)) :- !, print_term(A,[]).
interpret(_,nl) :- !, nl.
interpret(_,A is B) :- !, A is B.
% END
interpret(C,(P => B) ap A) :- !, copy_term(P=>B,A=>G), interpret(C,G).
interpret(_, F ap _) :- !, throw(error(not_lambda(F),_)).
interpret(_, P => B) :- !, throw(error(not_interpretable(P => B),_)).
interpret(C,H) :- match(C,H,B), interpret(C,B).

match(Clauses, Head, Body) :-
    member(Clause, Clauses),
    copy_term(Clause,C0),
    (C0 = (Head :- Body) -> true ; C0 = Head, Body = true).

subst(_,_,V,V) :- var(V), !.
subst(_,_,[],[]) :- !.
subst(P,A,[H|T],[H0|T0]) :- !, subst(P,A,H,H0), subst(P,A,T,T0).
subst(P,A,$P,A) :- !.
subst(_,_,B,B) :- constant(B), !.
subst(P,A,B,B0) :- B=..[F|Args], !, subst(P,A,Args,Args0), B0=..[F|Args0].

vars(V,[]) :- var(V), !.
vars($V,[V]) :- !.
vars([],[]) :- !.
vars([H|T],V) :- !, vars(H,VH), vars(T,VT), union(VH,VT,V).
vars(A,[]) :- constant(A), !.
vars(T,V) :- T=..[_|Args], vars(Args,V).

substfresh(T,T0) :- vars(T,Vs), substfresh(Vs,T,T0).
substfresh([],T,T) :- !.
substfresh([V|Vs],T,T1) :- !, subst(V,_Fresh,T,T0), substfresh(Vs,T0,T1).

constant(A) :- atom(A) ; number(A) ; string(A).

example_0 :-
    G = with(
        [
            female(alice),
            parent_child(alice,bob),
            parent_child(alice,charlie),
            member($a,[$a|_]),
            member($a,[_|$t]) :- member($a,$t),
            append([],$a,$a),
            append([$h|$t],$r,[$h|$z]) :- append($t,$r,$z),
            length([],0),
            (length([_|$t],$n) :- length($t,$m), $n is $m+1),
            true
        ],
        ([X,Y] => dump_context, female(X), parent_child(X,_), length([1,2],Y)) ap [P,Q]
    ), Vars=[P,Q],
    %G = member(A,[1,2,3]), Vars = [A],
    %G = (with([foo(a)], foo(A)), with([foo(b)], foo(B))), Vars=[A,B],
    substfresh(G,GS),
    print_term(G,[]),nl,
    print_term(GS,[]),nl,
    interpret([],GS),
    write(Vars),nl.

import(foo, [
    female(barbara),
    parent_child(barbara,daniel)
]).
