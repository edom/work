% Prolog argument reduction

reduce(A,_) :- var(A), !, false.
reduce(A+B,C) :- number(A), number(B), !, C is A+B.
reduce(A+B,C) :- string(A), string(B), !, string_concat(A,B,C).
reduce(A+B,C) :- string(A), number(B), !, string_concat(A,B,C).
reduce(A+B,C) :- number(A), string(B), !, string_concat(A,B,C).
reduce(A*B,C) :- number(A), number(B), !, C is A*B.
reduce(A+B,A0+B) :- reduce(A,A0), !.
reduce(A+B,A+B0) :- reduce(B,B0), !.
reduce(A*B,A0*B) :- reduce(A,A0), !.
reduce(A*B,A*B0) :- reduce(B,B0), !.
reduce(A=B,C) :- A == B, !, C = true.
reduce(_=_,C) :- !, C = false.
reduce(not(true),false) :- !.
reduce(not(false),true) :- !.
reduce(not(A),not(B)) :- !, reduce(A,B), !.
reduce(and(false,_),false) :- !.
reduce(and(true,A),A) :- !.
reduce(and(A,B),and(A0,B)) :- !, reduce(A,A0), !.
reduce(and(A,B),and(A,B0)) :- !, reduce(B,B0), !.
reduce(and([]),true) :- !.
reduce(and([false|_]),false) :- !.
reduce(and([true|A]),and(A)) :- !.
reduce(and(L),and(L0)) :- !, reduce(L,L0), !.
reduce([H|T],[H0|T]) :- reduce(H,H0), !.
reduce([H|T],[H|T0]) :- reduce(T,T0), !.
reduce(inc(A),B) :- number(A), !, B is A+1.
reduce(inc(A),inc(A0)) :- reduce(A,A0), !.
reduce(head([A|_]),B) :- !, A = B.
reduce(head(A),B) :- !, reduce(A,A0), !, B = head(A0).
reduce(map(_,[]),A) :- !, A = [].
reduce(map(F,[H|T]),R) :- !, R = [apply(F,H)|map(F,T)].
reduce(apply(Parm->Body, Arg),Sub) :- !, subst(Parm,Arg,Body,Sub).
reduce(apply(F,A),S) :- !, F=..L, append(L,A,M), S=..M.
reduce(let(Name,Rep,Body),S) :- !, subst(Name,Rep,Body,S).
% we may not want these
reduce(throw(E),_) :- !, throw(E).
reduce(read,T) :- !, read(T).
reduce(eval(E),F) :- !, eval(E,F).
reduce(println(A),B) :- !, eval(A,R), print(R), nl, B = unit.
reduce(sequence([]),unit) :- !.
reduce(sequence([H|T]),sequence(T)) :- !, eval(H,_).
reduce((A,B),C) :- !, eval(A,_), !, C = B.

    subst(P,_,let(P,R,B),S) :- !, S = let(P,R,B).
    subst(P,R,apply(P->B,A),S) :- !, subst(P,R,A,A0), S = apply(P->B,A0).
    subst(P,A,B,S) :- B = P, !, S = A.
    subst(_,_,[],S) :- !, S = [].
    subst(P,A,[H|T],S) :- !, subst(P,A,H,H0), subst(P,A,T,T0), S = [H0|T0].
    subst(P,A,B,S) :-
        functor_expansion(B,[FName|FArgs]),
        subst(P,A,FArgs,FArgs0),
        S =.. [FName|FArgs0],
        !.
    subst(_,_,B,S) :- !, S = B.

        functor_expansion(B,[FName|FArgs]) :-
            B =.. [FName|FArgs],
            B \= FName,
            \+member(FName,['[]', '[|]']).

eval(A,B) :- reduce(A,R), !, eval(R,B).
eval(A,A).

equiv(A,B) :- A == B.

printeval(A) :- print(A), nl.

goal_expansion(equiv(A,B),equiv(A0,B)) :- reduce(A,A0).
goal_expansion(equiv(A,B),equiv(A,B0)) :- reduce(B,B0).
goal_expansion(printeval(A), printeval(A0)) :- reduce(A,A0).
