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
:- use_module('./transput.pro').
:- use_module('./check0.pro').
:- use_module('./ps1_analysis_0.pro', except([routine_begin/2])).
:- use_module('./ps1_bit.pro').
:- use_module('./ps1_decompile.pro').
:- use_module('./ps1_disassemble.pro').
:- use_module('./ps1_exe.pro').
:- use_module('./ps1_memory.pro', except([memory_file/3])).
:- use_module('./ps1_procedural_simplify.pro', except([address_contains_constant/1])).
:- use_module('./ps1_ui.pro').

% See ps1_memory:memory_file/3.
:- multifile memory_file/3.
ps1_memory:memory_file(A, B, C) :- memory_file(A, B, C).

% See ps1_analysis_0:routine_begin/2.
:- multifile routine_begin/2.
:- dynamic routine_begin/2.
ps1_analysis_0:routine_begin(A, B) :- routine_begin(A, B).

% Infer routine_begin from EXE entry point.
routine_begin(Addr, Description) :-
    check_term(var + integer, Addr),
    exe_file(Path),
    exe_file__entry_point(Path, Addr),
    format(atom(Description), 'main; entry point; inferred from PS-X EXE file ~q', Path).

/*
address_contains_constant(Address) enables us to simplify loads from that Address into the value at that Address.
*/
:- multifile address_contains_constant/1.
ps1_procedural_simplify:address_contains_constant(A) :- address_contains_constant(A).

/*
Optional.
PS-X EXE file.
*/
:- multifile exe_file/1.

exe :-
    exe_file(Path),
    print_exe_info(Path).

% ========== interpreter output customization ==========

/*
portray/1 is a dynamic predicate that can change how the top-level writes terms.

Write all numbers in hexadecimal.
Add spaces around the assignment operator.
*/
portray(I) :- integer(I), integer_hex(I,H), write(H).
portray(A := B) :- format('~p := ~p', [A, B]).
