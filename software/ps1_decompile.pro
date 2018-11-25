:- use_module(library(clpfd)).
:- use_module(library(ugraphs)).
:- use_module('./map.pro').
:- use_module('./ps1_cpu.pro').

% Read 4-byte little endian as a Prolog integer.
get_4le(Stream, Word) :-
    get_byte(Stream, B0), B0 in 0..255,
    get_byte(Stream, B1), B1 in 0..255,
    get_byte(Stream, B2), B2 in 0..255,
    get_byte(Stream, B3), B3 in 0..255,
    Word #= (B3 << 24) \/ (B2 << 16) \/ (B1 << 8) \/ B0.

bytes_le__uint4([B0, B1, B2, B3], Word) :-
    ground(Word),
    !,
    split_bits(Word, 8, B321, B0),
    split_bits(B321, 8, B32, B1),
    split_bits(B32, 8, B3, B2).

bytes_le__uint4([B0, B1, B2, B3], Word) :-
    \+ ground(Word),
    !,
    Word #= (B3 << 24) \/ (B2 << 16) \/ (B1 << 8) \/ B0.

/*
End is exclusive.

Bytes is a list of bytes.
*/

ram_bytes(Begin, End, Bytes) :-
    Begin =< End,
    Byte_count #= End - Begin,
    File_offset #= Begin /\ 0x7fffffff,
    open('/home/erik/pcsxr/notes/vhnk/dump', read, Stream, [type(binary)]),
    seek(Stream, File_offset, bof, _),
    peek_string(Stream, Byte_count, String),
    string_codes(String, Bytes),
    close(Stream).

/*
A Block is a Label : Statements where Label is an integer and Statements is a list of Statement.

address_range__blocks(Begin, End, Blocks)
means that Statements represents the effect of executing the instruction from Begin inclusive to End exclusive.

The instruction sequence

    A: BRANCH somewhere
    B: SLOT
    C: ...

is roughly translated to the statement sequence

    A : [ slot, branch, goto(C) ]
    B : [ slot ]
    C : ...
*/
address_range__blocks(Begin, End, []) :- Begin #>= End, !.
address_range__blocks(Begin, End, [Begin : Branch_and_slot, Slot_begin : Slot | Rest]) :-
    address_instruction_friendly(Begin, Instruction),
    instruction_statements(Instruction, Now, Later),
    Later \= [], % a branch instruction
    !,
    Slot_begin #= Begin + 4,
    Slot_end #= Slot_begin + 4,
    Slot_end #=< End,
    substitute(pc, Begin, Now, Now0),
    substitute(pc, Slot_begin, Later, Later0),
    address_range__blocks(Slot_begin, Slot_end, [_ : Slot]),
    append(Slot0, [goto(_)], Slot), !, % Why does this create a choice point?
    append([Now0, Slot0, Later0, [goto(Slot_begin + 4)]], Branch_and_slot_0),
    statements_fix(Branch_and_slot_0, Branch_and_slot),
    Next #= Begin + 8,
    address_range__blocks(Next, End, Rest).
address_range__blocks(Begin, End, [Begin : Simple | Rest]) :-
    address_instruction_friendly(Begin, Instruction),
    instruction_statements(Instruction, Simple_0, []),
    % This ensures that every block has a branch target, for statement_branch_target/2.
    append(Simple_0, [goto(Begin + 4)], Simple_1),
    statements_fix(Simple_1, Simple),
    !,
    Next #= Begin + 4,
    address_range__blocks(Next, End, Rest).
address_range__blocks(Begin, End, [Begin : [error(Message)] | Rest]) :-
    address_word(Begin, Word),
    format(atom(Message), 'address_range__blocks: no instruction_statements/3 for 0x~16r at address 0x~16r', [Word, Begin]),
    Next #= Begin + 4,
    address_range__blocks(Next, End, Rest).

address_instruction_friendly(Address, Instruction) :-
    address_instruction(Address, I),
    instruction_friendly(I, Instruction).

/*
Evaluate constant-expression in goto.
Remove unreachable goto.
Forward jr_target.
*/
statements_fix([], []) :- !.

% Remove NOPs and unreachable gotos. These come from branch delay slots.
statements_fix([nop | A], OA) :- !, statements_fix(A, OA).
statements_fix([zr := _ | A], OA) :- !, statements_fix(A, OA).
statements_fix([T := E | A], [T := EE | OA]) :- !,
    substitute(zr, 0, E, E0),
    expression_simplified(E0, EE),
    statements_fix(A, OA).
statements_fix([goto(A) | _], [goto(EA)]) :- !, expression_simplified(A, EA).
statements_fix([if(C,T,F) | _], [if(C,ET,EF)]) :- !,
    expression_simplified(T, ET),
    expression_simplified(F, EF).
statements_fix([A | B], [A | C]) :- !, statements_fix(B, C).

/*
Simplify an expression, usually an arithmetic expression or a boolean expression.
*/
expression_simplified(true, true) :- !.
expression_simplified(false, false) :- !.
expression_simplified(I, V) :- integer(I), !, I = V.
expression_simplified([], []) :- !.
expression_simplified([A | B], [EA | EB]) :- !,
    expression_simplified(A, EA),
    expression_simplified(B, EB).
expression_simplified(A + 0, EA) :- !, expression_simplified(A, EA).
expression_simplified(0 + A, EA) :- !, expression_simplified(A, EA).
expression_simplified(Arith_exp, V) :-
    Arith_exp =.. [Op, A, B],
    member(Op, [+, -, *, <<, >>, /\, \/]),
    !,
    expression_simplified(A, EA),
    expression_simplified(B, EB),
    Clp_exp =.. [Op, EA, EB],
    ((integer(EA), integer(EB)) -> V #= Clp_exp; V = Clp_exp).
expression_simplified(Call, Out) :-
    Call =.. [Func | Args],
    !,
    expression_simplified(Args, Vals),
    Out =.. [Func | Vals].
expression_simplified(A, A) :- !.

/*
This heuristic assumes that every routine ends with JR RA.
The end is exclusive.
*/

routine_end(B, E) :-
    once((branch_from(B, X), address_instruction(X, jr(r(31))), E #= X + 8)).

/*
branch_from(Begin, Address) can be used to enumerate the branching instructions that come after Begin.
*/
branch_from(Begin, Begin) :- address_contains_branch(Begin).
branch_from(Begin, Address) :-
    Next #= Begin + 4,
    branch_from(Next, Address).

address_contains_branch(Address) :-
    address_instruction(Address, Instruction),
    instruction_statements(Instruction, _, [_ | _]).

/*
substitute(Sym, Rep, A, B)

Sym is an atom.

Rep is the term that replaces Sym.

A is the original term.

B is A with each occurrence of Sym replaced with Rep.
Functors are also replaced.
*/
substitute(Sym, Rep, Sym, Rep) :- !.
substitute(Sym, _, Atom, Atom) :- atom(Atom), !, Sym \= Atom.
substitute(_, _, Int, Int) :- integer(Int), !.
substitute(_, _, [], []) :- !.
substitute(Sym, Rep, [HA | A], [HB | B]) :- !, substitute(Sym, Rep, HA, HB), substitute(Sym, Rep, A, B).
substitute(Sym, Rep, F, G) :- F =.. FL, substitute(Sym, Rep, FL, GL), G =.. GL.

% This can be used to generate dword-aligned addresses between begin inclusive and end inclusive.
address4_between(Begin, End, Addr) :-
    Addr #>= Begin,
    Addr #< End,
    Addr mod 4 #= 0.

foreach_address4_between(Begin, End, Addr) :-
    address4_between(Begin, End, Addr),
    indomain(Addr).

address_word(Address, Word) :-
    End #= Address + 4,
    ram_bytes(Address, End, Bytes),
    bytes_le__uint4(Bytes, Word).

address_instruction(Address, Instruction) :-
    address_word(Address, Word),
    word_instruction(Word, Instruction).

/*
User interface.
*/

instruction_friendly(I, F) :-
    I =.. [Mnemonic | Args],
    map(A, B, operand_friendly(A, B), Args, F_args),
    F =.. [Mnemonic | F_args].

% Show disassembly.
disassemble(Begin) :-
    Max_ins #= 32,
    Max_end #= Begin + 4 * Max_ins,
    (routine_end(Begin, End0) -> true; End0 = Max_end),
    End #= min(End0, Max_end),
    disassemble(Begin, End).

disassemble(Begin, End) :-
    Begin #< End,
    address_word(Begin, Word),
    ((word_instruction(Word, I), instruction_friendly(I, Instruction)) -> true; Instruction = '???'),
    Instruction =.. [Mnemonic | Args],
    format('~|~`0t~16r~8+  ~|~`0t~16r~8+  ~p', [Begin, Word, Mnemonic]),
    map(A, format(' ~p', [A]), Args),
    nl,
    Next #= Begin + 4,
    disassemble(Next, End).

disassemble_routine(Begin) :-
    routine_end(Begin, End),
    disassemble(Begin, End).

/*
label_statements(Label, Statements) encodes the control flow graph.
Label is an integer address.
Statements is a list of Statements.
*/
:- dynamic label_statements/2.

routine_begin_assert(Begin) :-
    assertz(routine_begin(Begin, '')),
    format('Warning: decompile: implicitly assuming routine_begin(0x~16r).~n', [Begin]).

decompile(Begin) :-
    (routine_begin(Begin, Comment) -> true ; routine_begin_assert(Begin)),
    routine_end(Begin, End),
    format('Decompilation of routine 0x~16r -- 0x~16r (~w):~n', [Begin, End, Comment]),
    decompile(Begin, End).

decompile(Begin, End) :-
    format("Computing label_statements/2.~n"),
    retractall(label_statements(_, _)),
    address_range__blocks(Begin, End, Blocks),
    map(Label : Statements, assertz(label_statements(Label, Statements)), Blocks),
    format("Constructing control flow graph.~n"),
    findall(Vertex, label_statements(Vertex, _), Vertices),
    findall(A-B, label_successor(A, B), Edges),
    length(Vertices, NV),
    length(Edges, NE),
    format("Control flow graph contains ~w vertices and ~w edges.~n", [NV, NE]),
    format("Computing label_successors/2.~n"),
    map(A, A-Bs, findall(B, label_successor(A, B), Bs), Vertices, Graph),
    retractall(label_successors(_, _)),
    map(A-Bs, assertz(label_successors(A, Bs)), Graph),
    format("Computing domination relation and label_is_annexable/1.~n"),
    % This way of constructing Idoms takes O(V * E) time.
    % We want P to immediately dominate V.
    % We want P to be the only edge to V.
    map(V, P-V, findall(P, member(P-V, Edges), [P]), Vertices, Idoms),
    map(A-B, B, Idoms, Annexables),
    retractall(label_is_annexable(_)),
    map(A, assertz(label_is_annexable(A)), Annexables),
    annex(Vertices),
    % annex_dominated_blocks,
    %simplify_blocks,
    print_control_flow_graph.

% label_is_annexable(Label).
:- dynamic label_is_annexable/1.

:- dynamic label_successors/2.

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

print_control_flow_graph :-
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

statement_comment(goto(A), C) :- !, address_comment(A, C).
statement_comment(if(_,A,B), C) :- !,
    address_comment(A, CA),
    address_comment(B, CB),
    exclude(=(''), [CA, CB], Cs),
    atomic_list_concat(Cs, '; ', C).
statement_comment(_, '') :- !.

address_comment(A, C) :- routine_begin(A, C), !.
address_comment(_, '') :- !.

simplify_blocks :-
    forall(
        label_statements(A, B),
        (
            simplify_block(B, SB),
            retractall(label_statements(A, _)),
            assertz(label_statements(A, SB))
        )
    ).

/*
This assumes that the input is a basic block.

A basic block contains exactly one branch instruction.
The branch instruction the last instruction of the basic block.
*/
simplify_block([], []) :- !.
simplify_block([A | Rest], [A | Rest0]) :- !, simplify_block(Rest, Rest0).
% Collapse sequential writes. These are usually produced by loading 32-bit immediates (LUI followed by ORI, for example).
simplify_block([D := A, E := B | Rest], Rest0) :- !,
    expression_simplified(D, SD),
    expression_simplified(E, SE),
    expression_simplified(A, SA),
    expression_simplified(B, SB),
    substitute(SD, SA, SB, SB0),
    expression_simplified(SB0, SB1),
    (SD = SE -> Rest0 = Rest1 ; Rest0 = [SD := SA | Rest1]),
    simplify_block([SE := SB1 | Rest], Rest1).

/*
label_successor(A, B) means that B may be executed right after A.

This represents the adjacency list (the edge list) of the control flow graph.
*/
label_successor(A, B) :-
    label_statements(A, SA),
    member(S, SA),
    statement_branch_target(S, B),
    integer(B). % avoid jr_target

% statement_branch_target(Statement, Target) means Statement indicates a possible branch to Target.
statement_branch_target(ra := B, B).
statement_branch_target(goto(B), B).
statement_branch_target(if(_, B, _), B).
statement_branch_target(if(_, _, B), B).

statements__first_basic_block([], [], []) :- !.
statements__first_basic_block([goto(A) | Rest], [goto(A)], Rest) :- !.
statements__first_basic_block([if(A,B,C) | Rest], [if(A,B,C)], Rest) :- !.
statements__first_basic_block([A | Stas], [A | Blk], Rest) :- !, statements__first_basic_block(Stas, Blk, Rest).

statements__basic_blocks([], []) :- !.
statements__basic_blocks(Stas, [Blk | Blks]) :- !,
    statements__first_basic_block(Stas, Blk, Rest),
    statements__basic_blocks(Rest, Blks).

/*
Write all numbers in hexadecimal.
Add spaces around the assignment operator.
*/
portray(I) :- integer(I), format('0x~16r', [I]).
portray(A := B) :- format('~p := ~p', [A, B]).

routine_begin(A) :- routine_begin(A, _).

/*
Assumptions.

routine_begin(Address) asserts that Address is the address of the first instruction of a routine.

This is used to begin grouping the statement list into basic blocks.

We assume that the program always calls a routine from the beginning (never into the middle).
*/

:- dynamic routine_begin/2.

routine_begin(0x8001e034, 'called once per frame; computes outer product').
routine_begin(0x80062804, 'complex GTE stuff: lots of rtpt, nclip, avsz3, nccs, colors').
routine_begin(0x80065454, 'complex GTE stuff: lots of nclip, rtps, rgb, avsz4, nccs').
routine_begin(0x800694bc, '').
routine_begin(0x8006cc60, 'gte_load_rotation_matrix').
routine_begin(0x8006cc90, 'gte_load_translation_vector').
routine_begin(0x8006dac4, 'gte_load_rotation_and_translation').
routine_begin(0x8006dcdc, '').
routine_begin(0x8006dd00, 'do something with rotate_matrix_3').
routine_begin(0x8006dd0c, '').
routine_begin(0x8006d87c, '').
routine_begin(0x8006e054, 'rotate_matrix_3').
routine_begin(0x8006f5d0, '').
