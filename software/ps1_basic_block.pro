/** <module> basic block

The internal representation of a basic block is documented in bb_label_mid_last/4.
*/
:- module(ps1_basic_block, [
    bb_label_stas/3
    , bb_label_mid_last/4
    % unused
    , basic_block__last/2
    , basic_block__mid/2
    , basic_block__next/2
    , basic_block__nexts/2
    , basic_block__instructions/2
]).

:- use_module(library(clpfd)).

/** bb_label_stas(?Block, ?Label, ?Statements)

Convenience variant of bb_label_mid_last/4.
*/
bb_label_stas(Block, Lab, Ins) :- bb_label_mid_last(Block, Lab, Mid, Las), append(Mid, [Las], Ins), !.

/** bb_label_mid_last(?Block, ?Label, ?Mid, ?Last)

Label is anything unique.

Mid is a list of statements.

Last is a branch statement: either goto(Label) or if(Cond,TLabel,FLabel).

Design notes:
    - We separate Last to speed up traversing a chain of basic blocks.
*/
bb_label_mid_last(bb(Lab, Mid, Las), Lab, Mid, Las).

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
