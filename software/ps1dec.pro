/*
The library turns your Prolog interpreter into an interactive decompiler
for a memory dump produced by a PlayStation emulator running the game you want to reverse-engineer.

This file cannot be a module,
because we want the user to be able to manipulate this file's dynamic predicates without writing the module-colon prefix.
*/

% ========== user commands ==========

/*
disassemble(Address)
disassemble_routine(Address)
decompile(Address)
*/

% ========== the things the user has to do in order to use ps1_decompile ==========

% :- consult('ps1dec.pro').

:- use_module(library(clpfd)).
:- use_module('./ps1_analysis_0.pro', except([routine_begin/2])).
:- use_module('./ps1_decompile.pro').
:- use_module('./ps1_memory.pro', except([memory_file/3])).
:- use_module('./ps1_procedural_simplify.pro', except([address_contains_constant/1])).

/*
The user must map memory to file by asserting memory_file/3.

memory_file(Begin-End, Path, Offset) means the memory region Begin-End is mapped to file Path beginning at Offset.
End is exclusive.
All addresses and offsets count bytes.

The mappings must not overlap.
*/
:- multifile memory_file/3.
ps1_memory:memory_file(A, B, C) :- memory_file(A, B, C).

/*
routine_begin(Address, Comment) asserts that Address is the address of the first instruction of a routine.
This is used to begin grouping the statement list into basic blocks.
We assume that the program always calls a routine from the beginning (never into the middle).
*/
:- multifile routine_begin/2.
:- dynamic routine_begin/2.
ps1_analysis_0:routine_begin(A, B) :- routine_begin(A, B).

/*
address_contains_constant(Address) enables us to simplify loads from that Address into the value at that Address.
*/
:- multifile address_contains_constant/1.
ps1_procedural_simplify:address_contains_constant(A) :- address_contains_constant(A).

% ========== interpreter output customization ==========

/*
portray/1 is a dynamic predicate that can change how the top-level writes terms.

Write all numbers in hexadecimal.
Add spaces around the assignment operator.
*/
portray(I) :- integer(I), format('0x~16r', [I]).
portray(A := B) :- format('~p := ~p', [A, B]).
