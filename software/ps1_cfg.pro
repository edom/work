/** <module> control flow graph

CFG stands for "control flow graph".

CFG representation:
    - label_statements/2

Diagnostics:
    - cfg_format_debug/1
    - cfg_format_debug/2
*/
:- module(ps1_cfg, [
    label_statements/2
    , cfg_format_debug/1
    , cfg_format_debug/2
    , cfg_naive_from_blocks/1
    , cfg_assert/2
    , cfg_clear/0
    , cfg_annex_dominated_basic_blocks/0
    , cfg_print/0
    , cfg_print_size/0
    , cfg_simplify/0
]).

:- use_module('./map.pro').
:- use_module('./ps1_analysis_0.pro').
:- use_module('./ps1_procedural.pro').
:- use_module('./ps1_procedural_simplify.pro').

cfg_format_debug(Format) :- cfg_format_debug(Format, []).

:- multifile cfg_format_debug/2.
:- dynamic cfg_format_debug/2.
cfg_format_debug(Format, Args) :- format(Format, Args).


/** label_statements(?Label, ?Statements)

"The basic block labeled Label contains Statements."

Label is an integer address.

Statements is a list of =Statement=s.

The last Statement in the block is any of these:
    - goto(Addr)
    - if(Cond, AddrTrue, AddrFalse)
*/
:- dynamic label_statements/2.


/** label_successor(?A, ?B)

"B may be executed right after A."

This predicate represents the adjacency list (the edge list) of the control flow graph.
*/
label_successor(A, B) :-
    label_statements(A, SA),
    member(S, SA),
    statement_branch_target(S, B),
    integer(B). % avoid jr_target

label_predecessor(A, B) :- label_successor(B, A).
label_successors(A, Bs) :- findall(B, label_successor(A, B), Bs).
label_predecessors(A, Bs) :- findall(B, label_predecessor(A, B), Bs).
label_is_annexable(A) :- label_predecessors(A, [_]).

cfg_clear :- retractall(label_statements(_, _)).

cfg_assert(Label, Statements) :- assert(label_statements(Label, Statements)).

cfg_vertices(Vs) :-
    findall(Vertex, label_statements(Vertex, _), Vs).

cfg_naive_from_blocks(Blocks) :-
    cfg_format_debug("Constructing naive CFG (control flow graph).~n"),
    cfg_clear,
    map(Label : Statements, cfg_assert(Label, Statements), Blocks),
    cfg_print_size.

cfg_print_size :-
    cfg_vertices(Vertices),
    findall(A-B, label_successor(A, B), Edges),
    length(Vertices, NV),
    length(Edges, NE),
    format("Control flow graph now contains ~w vertices and ~w edges.~n", [NV, NE]).

cfg_annex_dominated_basic_blocks :-
    cfg_format_debug("Annexing dominated basic blocks.~n"),
    cfg_vertices(Vertices),
    annex(Vertices),
    cfg_print_size.

% This way of computing immediate dominators takes O(V * E) time.
% Merge blocks into their dominators.
annex([]) :- !.
annex([V | VS]) :-
    label_statements(V, _), !, % Ensure that this label isn't already annexed.
    annex_chain(V, Chain),
    map(L, S, label_statements(L, S), Chain, Stass),
    merge_statements_list(Stass, Stas),
    map(E, retractall(label_statements(E, _)), Chain),
    assertz(label_statements(V, Stas)),
    annex(VS).
annex([_ | VS]) :- !, annex(VS).

merge_statements_list([], []) :- !.
merge_statements_list([Stas], Stas) :- !.
merge_statements_list([[goto(_) | _] | Stass], Result) :- !, merge_statements_list(Stass, Result).
merge_statements_list([[H | Stas] | Stass], [H | Result]) :- !, merge_statements_list([Stas | Stass], Result).

annex_chain(Label, [Label | Chain]) :-
    label_successors(Label, [Suc]),
    label_is_annexable(Suc),
    !,
    annex_chain(Suc, Chain).
annex_chain(Label, [Label]) :- !.

cfg_print :-
    findall(Label, label_statements(Label, _), Labels),
    sort(Labels, Labels0),
    maplist(label_print, Labels0).

label_print(Label) :-
    label_statements(Label, [S | SS]),
    format(atom(Str), '0x~16r: ', [Label]),
    atom_length(Str, Indent),
    format('~w~p~n', [Str, S]),
    map(A, print_statement(Indent, A), SS).

print_statement(Indent, S) :-
    statement_comment(S, C),
    format('~|~t~*+~p', [Indent, S]),
    print_comment(C),
    nl.

print_comment('') :- !.
print_comment(C) :- format('  // ~w', [C]).

statement_comment(goto(A), C) :- !, label_comment(A, C).
statement_comment(if(_,A,B), C) :- !,
    label_comment(A, CA),
    label_comment(B, CB),
    exclude(=(''), [CA, CB], Cs),
    atomic_list_concat(Cs, '; ', C).
statement_comment(_, '') :- !.

label_comment(A, C) :- routine_begin(A, C), !.
label_comment(_, '') :- !.

cfg_simplify :-
    cfg_format_debug('Simplifying CFG by abstract interpretation.~n', []),
    cfg_vertices(Vertices),
    map(Vertex, cfg_simplify(Vertex), Vertices).

cfg_simplify(V) :-
    label_statements(V, Block0),
    % TODO continue the simplification to blocks dominated by V
    simplify_by_abstract_interpretation([], Block0, _, Block1),
    cfg_replace(V, Block1).

cfg_replace(V, B) :-
    retractall(label_statements(V, _)),
    assertz(label_statements(V, B)).
