/*
Bit manipulation.
*/
:- module(ps1_bit, [
    % bit manipulation
    split_bits/4
    , bytes_le__uint2/2
    , bytes_le__uint4/2
    , sint_integer/3
]).

:- use_module(library(clpfd)).

/*
split_bits(Whole, Index, Left, Right).

Index must be ground.

If Whole is not ground, then both [Left, Right] must be ground.

Take Index bits from right.

The length of Right is Index bits.

Left is whatever remains.

Example: split_bits(B4 B3 B2 B1 B0, 2, B4 B3 B2, B1 B0).
*/
split_bits(Whole, Index, Left, Right) :-
    ground(Whole), !,
    Left #= Whole >> Index,
    Right #= Whole /\ ((1 << Index) - 1).

split_bits(Whole, Index, Left, Right) :-
    \+ ground(Whole), !,
    Whole #= (Left << Index) \/ Right.

bytes_le__uint2([B0, B1], Half) :-
    ground(Half), !,
    split_bits(Half, 8, B1, B0).

bytes_le__uint2([B0, B1], Half) :-
    \+ ground(Half), !,
    Half #= (B1 << 8) \/ B0.

bytes_le__uint4([B0, B1, B2, B3], Word) :-
    ground(Word), !,
    split_bits(Word, 8, B321, B0),
    split_bits(B321, 8, B32, B1),
    split_bits(B32, 8, B3, B2).

bytes_le__uint4([B0, B1, B2, B3], Word) :-
    \+ ground(Word), !,
    Word #= (B3 << 24) \/ (B2 << 16) \/ (B1 << 8) \/ B0.

/*
This assumes that the Prolog implementation uses arbitrary-length integers.
*/

sint_integer(Bits, Sint, Integer) :-
    Sign_mask #= 1 << (Bits - 1),
    (Sint /\ Sign_mask #= 0 ->
        Integer = Sint
        ; Integer #= - ( ((\ Sint) + 1) /\ ((Sign_mask << 1) - 1))
    ).
