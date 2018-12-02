/** <module> PS1 decompiler state, especially RAM

RAM stands for "random access memory".

Mapping memory to file:
    - memory_file/3

Reading memory content:
    - address_byte/2
    - address_half/2
    - address_word/2

Decoding memory content:
    - address_instruction/2
*/
:- module(ps1_memory, [
    memory_file/3
    , address_byte/2
    , address_half/2
    , address_word/2
    , address_instruction/2
]).

:- use_module(library(clpfd)).
:- use_module('./interval.pro').
:- use_module('./map.pro').
:- use_module('./transput.pro').
:- use_module('./ps1_bit.pro').
:- use_module('./ps1_cpu.pro').

/** memory_file(?Region, ?Path, ?Offset).

"The memory Region is mapped to file Path beginning at Offset."

Region has the shape Begin-End.
Begin is inclusive.
End is exclusive.

All addresses and offsets count bytes.

The mappings must not overlap.

Example:
```
memory_file(0x80000000-0x80200000, 'my_ram_dump', 0)
```
*/
:- multifile memory_file/3.

/**
address_byte(?Address, ?Byte).
address_half(?Address, ?Half).
address_word(?Address, ?Word).

"The content of the RAM at Address is Byte/Half/Word."

Byte order is little-endian.

Example:
```
% Suppose these four clauses hold.
address_byte(0x80000000, 0x00).
address_byte(0x80000001, 0x01).
address_byte(0x80000002, 0x02).
address_byte(0x80000003, 0x03).
% This is what we mean by little-endian.
address_word(0x80000000, 0x03020100).
```
*/
address_byte(Address, Byte) :-
    End #= Address + 1,
    memory_bytes(Address-End, [Byte]).

address_half(Address, Half) :-
    End #= Address + 2,
    memory_bytes(Address-End, Bytes),
    bytes_le__uint2(Bytes, Half).

address_word(Address, Word) :-
    End #= Address + 4,
    memory_bytes(Address-End, Bytes),
    bytes_le__uint4(Bytes, Word).

/** address_instruction(?Address, ?Instruction).

"Decoding the instruction at Address produces Instruction, according to word_instruction/2."
*/
address_instruction(Address, Instruction) :-
    address_word(Address, Word),
    word_instruction(Word, Instruction).

% private

memory_bytes(Range, Bytes) :-
    memory_access_file_operations(Range, Sequence),
    map(access(_, Path, Begin, Count), Bytes, file_begin_count_bytes(Path, Begin, Count, Bytes), Sequence, Bytess),
    append(Bytess, Bytes).

/*
memory_access_file_operations/2 fails if there is any access outside the mapping defined by memory_file/3.
*/
memory_access_file_operations(A, []) :- interval_empty(A), !.
memory_access_file_operations(Range, [access(Beg-AEnd, Path, FBeg, FCount) | Acs]) :-
    memory_file(MBeg-MEnd, Path, Ofs),
    interval_non_empty(Range), Range = Beg-End,
    interval_intersection(Beg-End, MBeg-MEnd, Beg-AEnd),
    interval_non_empty(Beg-AEnd),
    !,
    FBeg #= Beg - MBeg + Ofs,
    FCount #= AEnd - Beg,
    memory_access_file_operations(AEnd-End, Acs).
