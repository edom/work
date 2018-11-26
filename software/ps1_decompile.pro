:- module(ps1_decompile, [
    decompile/1
    , decompile/2
]).

:- use_module(library(clpfd)).
:- use_module(library(ugraphs)).
:- use_module('./map.pro').
:- use_module('./ps1_analysis_0.pro').
:- use_module('./ps1_cfg.pro').
:- use_module('./ps1_procedural.pro').

decompile(Begin) :-
    ensure_routine_begin(Begin, Comment),
    routine_end(Begin, End),
    format('Decompilation of routine 0x~16r -- 0x~16r (~w):~n', [Begin, End, Comment]),
    decompile(Begin, End).

decompile(Begin, End) :-
    ensure_routine_begin(Begin, _),
    range_blocks(Begin-End, Blocks),
    cfg_naive_from_blocks(Blocks),
    cfg_annex_dominated_basic_blocks,
    cfg_simplify,
    cfg_annex_dominated_basic_blocks,
    cfg_print.

% This is important because routine_begin/2 affects basic-block leader detection.
ensure_routine_begin(Begin, Comment) :- routine_begin(Begin, Comment), !.
ensure_routine_begin(Begin, '') :-
    assertz(routine_begin(Begin, '')),
    format('Warning: decompile: implicitly assuming routine_begin(0x~16r).~n', [Begin]).
