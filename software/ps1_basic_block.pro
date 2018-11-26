/*
We encode a basic block as the compound term bb(Last_instruction).

The last instruction is a branch instruction: either goto(Label) or if(Cond,TLabel,FLabel).
*/
:- module(ps1_basic_block, [
    basic_block__last/2
    , basic_block__mid/2
    , basic_block__next/2
    , basic_block__nexts/2
    , basic_block__instructions/2
]).

:- use_module(library(clpfd)).

basic_block__mid(bb(A, _), A).

basic_block__last(bb(_, A), A).

basic_block__next(bb(_, goto(A)), A).
basic_block__next(bb(_, if(_, A, _)), A).
basic_block__next(bb(_, if(_, _, A)), A).

basic_block__nexts(A, Bs) :- findall(B, basic_block__next(A, B), Bs).

/*
This is bidirectional.
*/
basic_block__instructions(bb(M, L), R) :- append(M, [L], R).
