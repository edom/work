% implication as local assertion

:- op(1050,xfy,'=>'). % same as ->

:- dynamic scope_head_body/2.

interpret(A) :- Scope = 0, interpret(A,Scope).

interpret(true,_) :- !.
interpret(write(A),_) :- !, write(A).
interpret(nl,_) :- !, nl.
interpret((A,B),S) :- !, (interpret(A,S) , interpret(B,S)).
interpret((A;B),S) :- !, (interpret(A,S) ; interpret(B,S)).

% This leaves leftovers in scope_head_body/2.
interpret(Ante => Cons, Parent) :- !,
    Child is Parent+1,
    retractall(scope_head_body(Child,_,_)),
    assertall(Child, Ante),
    interpret(Cons, Child).

interpret(H, Cur) :- scope_head_body(S,H,B), S =< Cur, interpret(B,Cur).

assertall(_,A) :- var(A), !, instantiation_error(A).
assertall(S,(A,B)) :- !, assertall(S,A), assertall(S,B).
assertall(S,(A:-B)) :- !, assertz(scope_head_body(S,A,B)).
assertall(S,A) :- assertz(scope_head_body(S,A,true)).

test(0) :-
    interpret(
        (
            p(0),
            p(1)
            =>
            (
                p(2),
                p(3)
                => p(A) % A in {0,1,2,3}
            ),
            p(B), % B in {0,1}
            write(A-B), nl
        )
    ).

test(1) :-
    interpret(
        (
            edge(0,1),
            edge(1,2),
            (path(A,Z) :- edge(A,Z)),
            (path(A,Z) :- edge(A,M), path(M,Z))
            =>
            (
                edge(2,3)
                => path(A,Z), write(A-Z), nl
            )
        )
    ).

