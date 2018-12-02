:- module(ps1_analysis_0, [
    % routine-finding
    routine_begin/2
    , routine_begin/1
    % heuristics
    , routine_end/2
]).

:- use_module(library(clpfd)).
:- use_module('./ps1_cpu.pro').
:- use_module('./ps1_memory.pro').

/** routine_begin(Address, Comment)

"Address is the address of the first instruction of a routine."

This is used to begin grouping the statement list into basic blocks.
We assume that the program always calls a routine from the beginning (never into the middle).
*/
:- multifile routine_begin/2.
:- dynamic routine_begin/2.

routine_begin(A) :- routine_begin(A, _).

/*
This heuristic guesses the end of a routine.
We assume:
- Every routine has exactly one JR RA.
- Every routine ends with JR RA.
- Every routine does not exceed 100,000 instructions.

The end is exclusive.
*/
routine_end(B, E) :-
    for(I in 1 .. 100000),
    A #= B + 4 * I,
    address_instruction(A, jr(r(31))),
    !,
    E #= A + 8.

% private

for(I in Range) :- I in Range, indomain(I).
