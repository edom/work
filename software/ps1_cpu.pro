% MIPS R3000A with PlayStation 1 extensions.
% Far from done.

% http://hitmen.c02.at/files/docs/psx/psx.pdf

:- use_module('./map.pro').
:- use_module(library(clpfd)).

/*
gpr_name(Index, Name) means that the friendly name of general-purpose register Index is Name.
*/
gpr_name( 0, zr).
gpr_name( 1, at).
gpr_name( 2, v0).
gpr_name( 3, v1).
gpr_name( 4, a0).
gpr_name( 5, a1).
gpr_name( 6, a2).
gpr_name( 7, a3).
gpr_name( 8, t0).
gpr_name( 9, t1).
gpr_name(10, t2).
gpr_name(11, t3).
gpr_name(12, t4).
gpr_name(13, t5).
gpr_name(14, t6).
gpr_name(15, t7).
gpr_name(16, s0).
gpr_name(17, s1).
gpr_name(18, s2).
gpr_name(19, s3).
gpr_name(20, s4).
gpr_name(21, s5).
gpr_name(22, s6).
gpr_name(23, s7).
gpr_name(24, t8).
gpr_name(25, t9).
gpr_name(26, k0).
gpr_name(27, k1).
gpr_name(28, gp).
gpr_name(29, sp).
gpr_name(30, fp).
gpr_name(31, ra).

/*
split_bits(?Whole, +Index, ?Left, ?Right).

Index must be ground.

If Whole is not ground, then both [Left, Right] must be ground.

Take Index bits from right.

The length of Right is Index bits.

Left is whatever remains.

Example: split_bits(B4 B3 B2 B1 B0, 2, B4 B3 B2, B1 B0).
*/
split_bits(Whole, Index, Left, Right) :-
    ground(Whole), !,
    Left #= Whole >> Index,
    Right #= Whole /\ ((1 << Index) - 1).

split_bits(Whole, Index, Left, Right) :-
    \+ ground(Whole), !,
    Whole #= (Left << Index) \/ Right.

/*
Instruction formats.
https://en.wikipedia.org/wiki/MIPS_architecture#Instruction_formats
*/

/*
In instruction_op_param, if D is not ground, then the other variables must be ground.

In param_*, if P is not ground, then the other variables must be ground.

Example:
    instruction_op_param(D, Op, Param),
    param_r(Param, Rs, Rt, Rd, Sh, Fu).
*/

instruction_op_param(D, Op, Param) :-
    split_bits(D, 26, Op, Param).

param_r(P, Rs, Rt, Rd, Shamt, Funct) :-
    split_bits(P , 6, P1, Funct),
    split_bits(P1, 5, P2, Shamt),
    split_bits(P2, 5, P3, Rd),
    split_bits(P3, 5, P4, Rt),
    split_bits(P4, 5, 0, Rs).

param_i(P, Rs, Rt, Im) :-
    split_bits(P , 16, P1, Im),
    split_bits(P1, 5, P2, Rt),
    split_bits(P2, 5, 0, Rs).

param_j(P, Ix) :-
    split_bits(P , 26, 0, Ix).

/*
mnemonic_op_format(Mnemonic, Op, Format) relates instruction mnemonic, opcode, and format.

Format is any of these:
- r(Rs, Rt, Rd, Sh, Fu)
- j(Ix)
- i(Rs, Rt, Of)
Every variable in Format can be any of these:
- the atom 'any' (this represents no constraint)
- an integer (this represents equality constraint)

Shift instructions always shift the content of Rt;
the count is either Rs or Shamt.

Load and Store always uses rs for base memory address.
*/
mnemonic_op_format(sll, 0, r(0, any, any, any, 0)).
mnemonic_op_format(srl, 0, r(0, any, any, any, 2)).
mnemonic_op_format(sra, 0, r(0, any, any, any, 3)).
mnemonic_op_format(sllv, 0, r(any, any, any, 0, 4)).
mnemonic_op_format(srlv, 0, r(any, any, any, 0, 6)).
mnemonic_op_format(srav, 0, r(any, any, any, 0, 7)).
mnemonic_op_format(add, 0, r(any, any, any, 0, 32)).
mnemonic_op_format(addu, 0, r(any, any, any, 0, 33)).
mnemonic_op_format(sub, 0, r(any, any, any, 0, 34)).
mnemonic_op_format(subu, 0, r(any, any, any, 0, 35)).
mnemonic_op_format(and, 0, r(any, any, any, 0, 36)).
mnemonic_op_format(or, 0, r(any, any, any, 0, 37)).
mnemonic_op_format(xor, 0, r(any, any, any, 0, 38)).
mnemonic_op_format(nor, 0, r(any, any, any, 0, 39)).
mnemonic_op_format(slt, 0, r(any, any, any, 0, 42)).
mnemonic_op_format(sltu, 0, r(any, any, any, 0, 43)).
mnemonic_op_format(jr, 0, r(any, 0, 0, 0, 8)).
mnemonic_op_format(jalr, 0, r(any, 0, any, 0, 9)).
mnemonic_op_format(bltz, 1, i(any, 0, any)).
mnemonic_op_format(bgez, 1, i(any, 1, any)).
mnemonic_op_format(j, 2, j(any)).
mnemonic_op_format(jal, 3, j(any)).
mnemonic_op_format(beq, 4, i(any, any, any)).
mnemonic_op_format(bne, 5, i(any, any, any)).
mnemonic_op_format(blez, 6, i(any, 0, any)).
mnemonic_op_format(bgtz, 7, i(any, 0, any)).
mnemonic_op_format(addi, 8, i(any, any, any)).
mnemonic_op_format(addiu, 9, i(any, any, any)).
mnemonic_op_format(slti, 10, i(any, any, any)).
mnemonic_op_format(sltiu, 11, i(any, any, any)).
mnemonic_op_format(andi, 12, i(any, any, any)).
mnemonic_op_format(ori, 13, i(any, any, any)).
mnemonic_op_format(xori, 14, i(any, any, any)).
mnemonic_op_format(lui, 15, i(0, any, any)).
% mnemonic_op_format(cop2, 18, j(any)).
mnemonic_op_format(rtpt, 18, j(0x2280030)).
mnemonic_op_format(rtps, 18, j(0x2180001)).
mnemonic_op_format(mfc2, 18, i(0, any, any)).
mnemonic_op_format(cfc2, 18, i(2, any, any)).
mnemonic_op_format(mtc2, 18, i(4, any, any)).
mnemonic_op_format(ctc2, 18, i(6, any, any)).
mnemonic_op_format(lb, 32, i(any, any, any)).
mnemonic_op_format(lh, 33, i(any, any, any)).
mnemonic_op_format(lwl, 34, i(any, any, any)).
mnemonic_op_format(lw, 35, i(any, any, any)).
mnemonic_op_format(lbu, 36, i(any, any, any)).
mnemonic_op_format(lhu, 37, i(any, any, any)).
mnemonic_op_format(lwr, 38, i(any, any, any)).
mnemonic_op_format(sb, 40, i(any, any, any)).
mnemonic_op_format(sh, 41, i(any, any, any)).
mnemonic_op_format(swl, 42, i(any, any, any)).
mnemonic_op_format(sw, 43, i(any, any, any)).
mnemonic_op_format(swr, 46, i(any, any, any)).
mnemonic_op_format(lwc2, 0b110010, i(any, any, any)).
mnemonic_op_format(swc2, 0b111010, i(any, any, any)).

word_grouping(Word, i(Mnemonic, Op, Param)) :-
    instruction_op_param(Word, Op, Int_param),
    mnemonic_op_format(Mnemonic, Op, Params_constraint),
    params_match(Params_constraint, Int_param, Param).

params_match(r(CRs, CRt, CRd, CSh, CFu), P, r(Rs, Rt, Rd, Sh, Fu)) :-
    param_r(P, Rs, Rt, Rd, Sh, Fu),
    param__constraint_value(CRs, Rs),
    param__constraint_value(CRt, Rt),
    param__constraint_value(CRd, Rd),
    param__constraint_value(CSh, Sh),
    param__constraint_value(CFu, Fu).

params_match(i(CRs, CRt, CIm), P, i(Rs, Rt, Im)) :-
    param_i(P, Rs, Rt, Im),
    param__constraint_value(CRs, Rs),
    param__constraint_value(CRt, Rt),
    param__constraint_value(CIm, Im).

params_match(j(CIx), P, j(Ix)) :-
    param_j(P, Ix),
    param__constraint_value(CIx, Ix).

param__constraint_value(any, _).
param__constraint_value(A, A) :- integer(A).

/*
This predicate word_instruction/2 is mostly unidirectional.

Note the order Rs, Rt, Rd.
Rd comes last, unlike in MIPS assembly.
*/

word_instruction(Word, i(Mnemonic, NRs, NRt, NRd, Sh, Fu)) :-
    word_grouping(Word, i(Mnemonic, _, r(Rs, Rt, Rd, Sh, Fu))),
    gpr_name(Rs, NRs),
    gpr_name(Rt, NRt),
    gpr_name(Rd, NRd).

word_instruction(Word, i(Mnemonic, NRs, NRt, Im)) :-
    word_grouping(Word, i(Mnemonic, _, i(Rs, Rt, Im))),
    gpr_name(Rs, NRs),
    gpr_name(Rt, NRt).

word_instruction(Word, i(Mnemonic, Ix)) :-
    word_grouping(Word, i(Mnemonic, _, j(Ix))).

word_instruction_tolerant(Word, Ins) :-
    word_instruction(Word, Ins) -> true ; Ins = i('???').

instruction_mnemonic(F, M) :- functor(F, i, _), arg(1, F, M).

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
This assumes that the Prolog implementation uses arbitrary-length integers.
*/

sint_integer(Bits, Sint, Integer) :-
    Sign_mask #= 1 << (Bits - 1),
    (Sint /\ Sign_mask #= 0 ->
        Integer = Sint
        ; Integer #= - ( ((\ Sint) + 1) /\ ((Sign_mask << 1) - 1))
    ).

/*
Evaluate an expression, usually an arithmetic expression or a boolean expression.
*/

eval(true, true) :- !.
eval(false, false) :- !.
eval(I, V) :- integer(I), !, I = V.
eval([], []) :- !.
eval([A | B], [EA | EB]) :- !, eval(A, EA), eval(B, EB).
eval(Arith_exp, V) :-
    Arith_exp =.. [Op, A, B],
    member(Op, [+, -, *, <<, >>, /\, \/]),
    !,
    eval(A, EA),
    eval(B, EB),
    Clp_exp =.. [Op, EA, EB],
    V #= Clp_exp.
eval(Bool_exp, V) :-
    Bool_exp =.. [Op, A, B],
    member(Op, [=, \=]),
    !,
    eval(A, EA),
    eval(B, EB),
    (call(Op, EA, EB) -> V = true ; V = false).
eval(if(Cond, True, False), Val) :- !, (eval(Cond, true) -> Val = True ; Val = False).
eval(Call, V) :-
    Call =.. [Func | Args],
    length(Args, FArity),
    LArity #= FArity + 1,
    current_predicate(Func / LArity),
    !,
    eval(Args, Vals),
    Goal =.. [Func | Vals],
    call(Goal, Out),
    eval(Out, V).
eval(A, A) :- !.



/*
Decompiling from instructions to abstract-procedural-language statements.

We assume these to simplify the decompiler:

Branch delay slot does not contain branch-delay-slot-producing instruction.

Loads and stores finish immediately; no load delay slot.
Memory and coprocessor access are instantaneous.

https://github.com/aquynh/capstone/issues/209
"Processor-operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is Placed in the delay slot of a branch or jump."
*/

/*
instruction_statements/2
instruction_statements(Instruction, Now) describes the effects of a non-branch instruction.
*/
instruction_statements(i(addu, Rs, Rt, Rd, _, _), [Rd := Rs + Rt]).
instruction_statements(i(addiu, Rs, Rt, Im), [Rt := Rs + Val]) :- sint_integer(16, Im, Val).
instruction_statements(i(subu, Rs, Rt, Rd, _, _), [Rd := Rs - Rt]).
instruction_statements(i(and, Rs, Rt, Rd, _, _), [Rd := Rs /\ Rt]).
instruction_statements(i(andi, Rs, Rt, Im), [Rt := Rs /\ Im]).
instruction_statements(i(or, Rs, Rt, Rd, _, _), [Rd := Rs \/ Rt]).
instruction_statements(i(lui, _, Rt, Im), [Rt := Val]) :- Val #= Im << 16.
instruction_statements(i(ori, Rs, Rt, Im), [Rt := Rs \/ Im]).
instruction_statements(i(lw, Rs, Rt, Im), [Rt := m4(Rs + Im)]).
instruction_statements(i(sw, Rs, Rt, Im), [m4(Rs + Im) := Rt]).
instruction_statements(i(sll, _, Rt, Rd, Sh, _), [Rd := Rt << Sh]).
instruction_statements(i(srl, _, Rt, Rd, Sh, _), [Rd := Rt >> Sh]).
instruction_statements(i(sltiu, Rs, Rt, Im), [Rt := Rs < Val]) :- sint_integer(16, Im, Val).
/*
instruction_statements/3
instruction_statements(Instruction, Now, Later) describes the effects of a branch instruction.

PC in Now is the address of the instruction itself, not added by four.
*/
instruction_statements(i(j, Ix), [], [goto((pc /\ 0xf0000000) \/ Ix2)]) :- Ix2 #= Ix << 2.
instruction_statements(i(jr, Rs, _, _, _, _), [jr_target := Rs], [goto(jr_target)]).
instruction_statements(i(jal, Ix), [ra := pc + 8], [goto((pc /\ 0xf0000000) \/ Ix2)]) :- Ix2 #= Ix << 2.
instruction_statements(i(beq, Rs, Rt, Im), [condition := (Rs = Rt)], [if(condition, goto(pc + Ofs))]) :- im16_ofs(Im, Ofs).
instruction_statements(i(bne, Rs, Rt, Im), [condition := (Rs \= Rt)], [if(condition, goto(pc + Ofs))]) :- im16_ofs(Im, Ofs).

% Translate 16-bit instruction immediate to PC offset.
im16_ofs(Ins_count, Ofs) :-
    Byte_count #= Ins_count << 2,
    sint_integer(18, Byte_count, Ofs).

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
    address_instruction(Begin, Instruction),
    instruction_statements(Instruction, Now, Later), % a branch instruction
    !,
    Slot_begin #= Begin + 4,
    Slot_end #= Slot_begin + 4,
    Slot_end #=< End,
    substitute(pc, Begin, Now, Now0),
    substitute(pc, Slot_begin, Later, Later0),
    address_range__blocks(Slot_begin, Slot_end, [_ : Slot]),
    append([Now0, Slot, Later0, [goto(Slot_begin + 4)]], Branch_and_slot),
    Next #= Begin + 8,
    address_range__blocks(Next, End, Rest).
address_range__blocks(Begin, End, [Begin : Simple | Rest]) :-
    address_instruction(Begin, Instruction),
    instruction_statements(Instruction, Simple), % not a branch instruction
    !,
    Next #= Begin + 4,
    address_range__blocks(Next, End, Rest).

/*
A Statement is an abstract-procedural-language statement.
*/

address_range__statements(Begin, End, Statements) :-
    address_range__blocks(Begin, End, Blocks),
    map(Block, Stmts, block_statements(Block, Stmts), Blocks, Stmtss),
    flatten(Stmtss, Statements).

block_statements(Label : Statements, [label(Label) | Statements]).

optimize([], []) :- !.

% Remove NOPs and unreachable gotos. These come from branch delay slots.
optimize([zr := _ | A], OA) :- !, optimize(A, OA).
optimize([goto(A), goto(_) | B], C) :- !, optimize([goto(A) | B], C).

optimize([goto(A) | B], [goto(EA) | OB]) :- !, eval(A, EA), optimize(B, OB).
optimize([if(C,goto(L)) | B], [if(C,goto(EL)) | OB]) :- !, eval(L, EL), optimize(B, OB).

optimize([A | B], [A | C]) :- !, optimize(B, C).

/*
Assumptions.

routine_begin(Address) asserts that Address is the address of the first instruction of a routine.

This is used to begin grouping the statement list into basic blocks.
*/

routine_begin(0x8006cc60).
routine_begin(0x8006cc90).
routine_begin(0x8006dac4).

/*
This heuristic assumes that every routine ends with JR RA.
The end is exclusive.
*/

routine_end(B, E) :-
    once((branch_from(B, X), address_instruction(X, i(jr, ra, _, _, _, _)), E #= X + 8)).

/*
branch_from(Begin, Address) can be used to enumerate the branching instructions that come after Begin.
*/
branch_from(Begin, Begin) :- address_contains_branch(Begin).
branch_from(Begin, Address) :-
    Next #= Begin + 4,
    branch_from(Next, Address).

address_contains_branch(Address) :-
    address_instruction(Address, Instruction),
    instruction_statements(Instruction, _, _).

print_list(List) :- map(A, format("~p~n", [A]), List).

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
    word_instruction_tolerant(Word, Instruction).

/*
User interface.
*/

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
    word_instruction_tolerant(Word, Instruction),
    format("~|~`0t~16r~8+  ~|~`0t~16r~8+  ~p~n", [Begin, Word, Instruction]),
    Next #= Begin + 4,
    disassemble(Next, End).

decompile(Begin) :-
    routine_end(Begin, End),
    decompile(Begin, End).

decompile(Begin, End) :-
    address_range__statements(Begin, End, Statements),
    optimize(Statements, Out),
    print_list(Out).

% Write all numbers in hexadecimal.
portray(I) :- integer(I), format('0x~16r', [I]).
