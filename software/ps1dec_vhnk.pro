% Reverse engineering "Virtual hiryuu no ken".

:- consult('ps1dec.pro').

exe_file('./SLPS_003.38').
memory_file(0x80000000-0x80200000, '/home/erik/pcsxr/notes/vhnk/dump', 0).

/*
region(Begin, End, Description) means that a region begins at address Begin (inclusive) and ends at address End (exclusive).
Every instruction whose address is >= Begin and < End is in the region.
End is exclusive.
The instruction at End - 8 should be JR $RA.
The Description is for human use.

address_region(Addr, Begin, End, Desc) means that instruction at address Addr is part of the region (see region/3).

address_region(Addr, Begin) is a convenience for address_region(Addr, Begin, _, _).

address_comment(Address, Comment) is our note about a memory address.
*/

/*
actor_read4(PC, Src) means that we observe that instruction at address PC reads from memory address Src.
*/

/*
actor_write4(PC, Dst) means that we observe that instruction at address PC writes to memory address Dst.
*/

/*
caller_callee(Src, Dst) means that we observe that the JAL instruction at address Src calls the instruction at address Dst.
*/

/*
address_per_frame(A, N) means the instruction at address A gets executed N times every video frame.
*/

:- dynamic region/3.

actor_write4(0x8006f858, 0x1f800140).
caller_callee(0x8001e968, 0x8006f5d0).

address_region(Addr, Begin, End, Desc) :-
    Addr #>= Begin,
    Addr #< End,
    region(Begin, End, Desc).

address_region(Addr, Begin) :- address_region(Addr, Begin, _, _).

% psx_run_to 0x8006d87c
address_per_frame(0x8006d87c, 1).

address_comment(0x1f800140, 'matrix').
address_comment(0x1f800154, 'matrix TRX').
address_comment(0x8006d87c, 'called once per frame; we think this is the main frame loop').
address_comment(0x8006cc9c, 'CTC2 TRX').

format_address(Addr) :-
    format('~16r', [Addr]),
    (address_comment(Addr, Comment) -> format(' (~w)', Comment) ; true),
    (address_region(Addr, Begin, End, Desc) -> format(' (~16r-~16r ~w)', [Begin, End, Desc]) ; true).

format_caller(Addr) :-
    address_region(Addr, Routine),
    caller_callee(Caller, Routine),
    !,
    format_address(Addr),
    write(' is called by '),
    format_address(Caller),
    nl,
    format_caller(Caller).

format_caller(Addr) :-
    format('~16r is called by unknown~n', [Addr]).

% high-level queries.
who_writes_to(Mem) :-
    actor_write4(Actor, Mem),
    format_address(Actor),
    write(' writes to '),
    format_address(Mem),
    nl,
    format_caller(Actor),
    nl.

routine_begin(0x8001e034, '? called once per frame; computes outer product').
routine_begin(0x80062804, 'complex GTE stuff: lots of rtpt, nclip, avsz3, nccs, colors').
routine_begin(0x80065454, 'complex GTE stuff: lots of nclip, rtps, rgb, avsz4, nccs').
routine_begin(0x800694bc, '? calls many subroutines').
routine_begin(0x8006cc60, 'gte_load_rotation_matrix').
routine_begin(0x8006cc90, 'gte_load_translation_vector').
routine_begin(0x8006dac4, 'gte_load_rotation_and_translation').
routine_begin(0x8006dcdc, '?').
routine_begin(0x8006dd00, '? do something with rotate_matrix_3').
routine_begin(0x8006dd0c, '?').
routine_begin(0x8006d87c, '?').
routine_begin(0x8006e054, 'rotate_matrix_3').
routine_begin(0x8006f5d0, '? copy what to the matrix area?').

% address_contains_constant(_).
