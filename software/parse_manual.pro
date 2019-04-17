/*
This is an expression parser with these features:
- manual left-recursion elimination
- dynamic infix operator with associativity and precedence
*/

/*
The left-recursion elimination is the transformation from
    A --> A,B ; C
to
    A --> C, many0(B)
where B must not begin with A
and C must not begin with A.

This file contains a manual left-recursion elimination of this rule:

    exp --> exp, ("+", exp ; "*", exp).
    exp --> "(", exp, ")" ; num.

into this rule:

    exp --> expl, many0(expr).
    expl --> "(", exp, ")".
    expl --> num.
    expr -->

and then we add some infix binary operator parsing with precedence and associativity.
*/

:- export(exp//1).

exp(E) --> expl(L), exprs(L,E).
expl(par(A)) --> "(", exp(A), ")".
expl(num(A)) --> num(A).
expr(L,bin(O,L,R)) -->
    {operator(OO,OA,O)},
    {
        exp_ord(L,OL),
        (   (OA=yfx;OA=yfy) -> OL=<OO
        ;   (OA=xfx;OA=xfy) -> OL<OO
        )
    },
    {atom_codes(O,OCodes)},
    OCodes,
    exp(R),
    {
        exp_ord(R,OR),
        (   (OA=xfy;OA=yfy) -> OR=<OO
        ;   (OA=xfx;OA=yfx) -> OR<OO
        )
    }.
exprs(L,L) --> "".
exprs(L,E) --> expr(L,M), exprs(M,E).

dig(0) --> "0".
dig(1) --> "1".
dig(2) --> "2".
dig(3) --> "3".
dig(4) --> "4".
dig(5) --> "5".
dig(6) --> "6".
dig(7) --> "7".
dig(8) --> "8".
dig(9) --> "9".

num(D) --> many1(dig,D).

many0(_,[]) --> "".
many0(A,[H|T]) --> call(A,H), many0(A,T).

many1(A,[H]) --> call(A,H).
many1(A,[H|T]) --> call(A,H), many1(A,T).

:- dynamic operator/3.
:- retractall(operator(_,_,_)).
:- assertz(operator(400,yfx,*)).
:- assertz(operator(500,yfx,+)).
:- assertz(operator(500,yfx,-)).

op_ord(A,B) :- once(operator(B,_,A)).

exp_ord(par(_),A) :- !, A=0.
exp_ord(num(_),A) :- !, A=0.
exp_ord(bin(Op,_,_),A) :- !, op_ord(Op,A).
exp_ord(A,_) :- domain_error(exp,A).

exp_meaning(par(A),B) :- !, exp_meaning(A,B).
exp_meaning(num(A),B) :- !, reverse(A,R), num_meaning(R,B).
exp_meaning(bin(Op,A,B),Z) :- !, exp_meaning(A,A0), exp_meaning(B,B0), Z =.. [Op,A0,B0].
exp_meaning(E,_) :- !, domain_error(exp,E).

num_meaning([],0).
num_meaning([A|B],C) :- num_meaning(B,N), C is 10*N+A.

test :-
    string_codes("12+34*56-78",Codes),
    phrase(exp(A),Codes),
    print_term(A,[]),nl,
    exp_meaning(A,M),
    print_term(M,[]),nl.

