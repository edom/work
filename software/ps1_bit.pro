/** <module> bit manipulation

*/
:- module(ps1_bit, [
    % bit manipulation
    split_bits/4
    , bytes_le__uint2/2
    , bytes_le__uint4/2
    , sint_integer/3
    , integer_hex/2
]).

:- use_module(library(clpfd)).

/** split_bits(?Whole, +Index, ?Left, ?Right)

"Splitting Whole at bit Index produces Left and Right, where Right is the Index least significant bits of Whole."

Every argument is an integer.

Bit 0 is the rightmost bit, the least significant bit.

Groundness requirements:
    - Index must be ground.
    - At least two of [Whole, Left, Right] must be ground.

Another way to understand the predicate:
    - Right is the Index least significant bits of Whole.
    - Left is whatever remains.

Example:
```
% split_bits(B4 B3 B2 B1 B0, 2, B4 B3 B2, B1 B0).
split_bits(0b101_11, 2, 0b101, 0b11).
```
*/
split_bits(Whole, Index, Left, Right) :-
    integer(Whole), !,
    Left #= Whole >> Index,
    Right #= Whole /\ ((1 << Index) - 1).

split_bits(Whole, Index, Left, Right) :-
    \+ ground(Whole), !,
    Whole #= (Left << Index) \/ Right.

split_bits(A,B,C,D) :- throw(error(split_bits(A,B,C,D), _)).

bytes_le__uint2([B0, B1], Half) :-
    integer(Half), !,
    split_bits(Half, 8, B1, B0).

bytes_le__uint2([B0, B1], Half) :-
    \+ ground(Half), !,
    Half #= (B1 << 8) \/ B0.

bytes_le__uint4([B0, B1, B2, B3], Word) :-
    integer(Word), !,
    split_bits(Word, 8, B321, B0),
    split_bits(B321, 8, B32, B1),
    split_bits(B32, 8, B3, B2).

bytes_le__uint4([B0, B1, B2, B3], Word) :-
    \+ ground(Word), !,
    Word #= (B3 << 24) \/ (B2 << 16) \/ (B1 << 8) \/ B0.

bytes_le__uint4(A,B) :- throw(error(bytes_le__uint4(A,B), _)).

/*
This assumes that the Prolog implementation uses arbitrary-length integers.
*/

sint_integer(Bits, Sint, Integer) :-
    Sign_mask #= 1 << (Bits - 1),
    (Sint /\ Sign_mask #= 0 ->
        Integer = Sint
        ; Integer #= - ( ((\ Sint) + 1) /\ ((Sign_mask << 1) - 1))
    ).

/** integer_hex(?Int, ?Hex)

"Word is Hex hexadecimal."

Int is integer.

Hex is string.

At least one parameter must be bound.

Example:
```
integer_hex(16,"0x10").
integer_hex(-16,"-0x10").
```
*/
integer_hex(Word, Hex) :- integer(Word), Word >= 0, !, format(string(Hex), '0x~16r', [Word]).
integer_hex(Word, Hex) :- integer(Word), Word < 0, !, Abs is -Word, format(string(Hex), '-0x~16r', [Abs]).
integer_hex(Word, Hex) :- string(Hex), !, number_string(Word, Hex).
integer_hex(Word, Hex) :- throw(error(invalid_arguments(Word, Hex), _)).
