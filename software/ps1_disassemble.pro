:- module(ps1_disassemble, [
    disassemble/1
    , disassemble/2
    , disassemble_routine/1
]).

:- use_module(library(clpfd)).
:- use_module('./map.pro').
:- use_module('./ps1_analysis_0.pro').
:- use_module('./ps1_cpu.pro').
:- use_module('./ps1_memory.pro').

% Show disassembly.
disassemble(Begin) :-
    Max_ins #= 32,
    Max_end #= Begin + 4 * Max_ins,
    (routine_end(Begin, End0) -> true; End0 = Max_end),
    End #= min(End0, Max_end),
    disassemble(Begin, End).

disassemble(Begin, Begin) :- !.
disassemble(Begin, End) :-
    Begin #< End,
    address_word(Begin, Word),
    word_instruction_robust(Word, Instruction),
    Instruction =.. [Mnemonic | Args],
    format('~|~`0t~16r~8+  ~|~`0t~16r~8+  ~p', [Begin, Word, Mnemonic]),
    map(A, format(' ~p', [A]), Args),
    nl,
    Next #= Begin + 4,
    disassemble(Next, End).

word_instruction_robust(Word, Instruction) :-
    word_instruction(Word, I), !,
    instruction_friendly(I, Instruction).

word_instruction_robust(_, '???').

disassemble_routine(Begin) :-
    routine_end(Begin, End),
    disassemble(Begin, End).
