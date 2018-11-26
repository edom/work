/*
PS1 decompiler state.
*/
:- module(ps1_memory, [
    % mapping
    memory_file/3
    % memory access
    , address_byte/2
    , address_half/2
    , address_word/2
    , address_instruction/2
]).

:- use_module(library(clpfd)).
:- use_module('./interval.pro').
:- use_module('./map.pro').
:- use_module('./ps1_bit.pro').
:- use_module('./ps1_cpu.pro').

% See memory_file/3 in ps1_decompile.
:- multifile memory_file/3.

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

address_instruction(Address, Instruction) :-
    address_word(Address, Word),
    word_instruction(Word, Instruction).

% private

memory_bytes(Range, Bytes) :-
    memory_access_file_operations(Range, Sequence),
    map(access(_, Path, Begin, Count), Bytes, file_bytes(Path, Begin, Count, Bytes), Sequence, Bytess),
    append(Bytess, Bytes).

file_bytes(Path, Begin, Count, Bytes) :-
    open(Path, read, Stream, [type(binary)]),
    seek(Stream, Begin, bof, _),
    peek_string(Stream, Count, String),
    string_codes(String, Bytes),
    close(Stream).

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
