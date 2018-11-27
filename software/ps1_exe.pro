/*
See also:
- struct XF_HDR in psyq/include/KERNEL.H
- https://patpend.net/technical/psx/exeheader.txt
*/
:- module(ps1_exe, [
    print_exe_info/1
]).

:- use_module('./transput.pro').
:- use_module('./ps1_bit.pro').

print_exe_info(Path) :-
    with_file(Path, read, [type(binary)], Stream, print_exe_info_(Stream)).

print_exe_info_(Stream) :-
    stream_pc0(Stream, Pc0),
    stream_text(Stream, TAddr, TSize),
    stream_data(Stream, DAddr, DSize),
    stream_bss(Stream, BAddr, BSize),
    stream_stack(Stream, SAddr, SSize),
    format('\
entry point

    pc0   ~16r

segments

    text  ~16r ~16r
    data  ~16r ~16r
    bss   ~16r ~16r
    stack ~16r ~16r
', [Pc0, TAddr, TSize, DAddr, DSize, BAddr, BSize, SAddr, SSize]).

stream_pc0(Stream, Pc0) :- stream_begin_count_value_(Stream, 16, 4, [uint32(Pc0)]).
stream_text(Stream, Addr, Size) :- stream_begin_count_value_(Stream, 24, 8, [uint32(Addr), uint32(Size)]).
stream_data(Stream, Addr, Size) :- stream_begin_count_value_(Stream, 32, 8, [uint32(Addr), uint32(Size)]).
stream_bss(Stream, Addr, Size) :- stream_begin_count_value_(Stream, 40, 8, [uint32(Addr), uint32(Size)]).
stream_stack(Stream, Addr, Size) :- stream_begin_count_value_(Stream, 48, 8, [uint32(Addr), uint32(Size)]).

stream_begin_count_value_(_, _, 0, []) :- !.
stream_begin_count_value_(Stream, Position0, Count2, [H | T]) :- !,
    stream_begin_count_value_(Stream, Position0, Count0, H),
    Position1 is Position0 + Count0,
    stream_begin_count_value_(Stream, Position1, Count1, T),
    Count2 is Count0 + Count1.

stream_begin_count_value_(Stream, Position, 4, uint32(Value)) :- !,
    stream_begin_count_bytes(Stream, Position, 4, A),
    bytes_le__uint4(A, Value).
