% functional

apply(Param^Body, Arg) :-
    substvar(Param, Arg, Body, Subed),
    call(Subed).

% substvar(?Var, +Sub, +Exp, -Res) is det.
substvar(V,S,E,R) :- V == E, !, S = R.
substvar(_,_,E,R) :- var(E), !, E = R.
substvar(_,_,E,R) :- ground(E), !, E = R.
substvar(V,S,E,R) :- !,
    functor(E,Name,Arity),
    functor(R,Name,Arity),
    foreach(between(1,Arity,N), subst_arg(N,V,S,E,R)).

subst_arg(N,V,S,E,R) :-
    arg(N,E,A),
    arg(N,R,B),
    substvar(V,S,A,B).

example_0 :-
    apply(A^writeln(A+1), 2).

example_1 :-
    apply(A^apply(B^writeln(A+B), 2), 1).

% logic-relational "local predicate"

example_2 :-
    ( (p(x) ; p(y) ; p(z) ) , (q(0) ; q(1) ; q(2)) )
    ^
    (p(A), q(B), writeln(A+B)).

(H^B) :- assume(H,B,B0), call(B0).

assume((A,B),C,Z) :- !, assume(A,C,D), assume(B,D,Z).
assume((A;B),C,Z) :- !, (assume(A,C,Z) ; assume(B,C,Z)).
assume(H,(A,B),Z) :- !, Z = (A0,B0), assume(H,A,A0), assume(H,B,B0).
assume(H,(A;B),Z) :- !, Z = (A0;B0), assume(H,A,A0), assume(H,B,B0).
assume(H,H,true).
assume(H,A,A) :- \+ similar(H,A).

similar(F,G) :- functor(F,N,A), functor(G,N,A).

