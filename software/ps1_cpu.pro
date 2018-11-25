/*
MIPS R3000A with PlayStation 1 extensions.
Far from done.

- http://hitmen.c02.at/files/docs/psx/psx.pdf
- https://s3-eu-west-1.amazonaws.com/downloads-mips/documents/MD00086-2B-MIPS32BIS-AFP-6.06.pdf
- https://en.wikipedia.org/wiki/MIPS_architecture#Instruction_formats
*/

:- module(ps1_cpu, [
    word_instruction/2
    , instruction_statements/3
    , operand_friendly/2
    , split_bits/4
]).

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

cp2cr_name( 0, gc_r11r12).
cp2cr_name( 1, gc_r13r21).
cp2cr_name( 2, gc_r22r23).
cp2cr_name( 3, gc_r31r32).
cp2cr_name( 4, gc_r33).
cp2cr_name( 5, gc_trx).
cp2cr_name( 6, gc_try).
cp2cr_name( 7, gc_trz).
cp2cr_name( 8, gc_l11l12).
cp2cr_name( 9, gc_l13l21).
cp2cr_name(10, gc_l22l23).
cp2cr_name(11, gc_l31l32).
cp2cr_name(12, gc_l33).
cp2cr_name(13, gc_rbk).
cp2cr_name(14, gc_gbk). % 14 and 15 are swapped in Joshua Walker's documentation?
cp2cr_name(15, gc_bbk).
cp2cr_name(16, gc_lr1lr2).
cp2cr_name(17, gc_lr3lg1).
cp2cr_name(18, gc_lg2lg3).
cp2cr_name(19, gc_lb1lb2).
cp2cr_name(20, gc_lb3).
cp2cr_name(21, gc_rfc).
cp2cr_name(22, gc_gfc).
cp2cr_name(23, gc_bfc).
cp2cr_name(24, gc_ofx).
cp2cr_name(25, gc_ofy).
cp2cr_name(26, gc_h).
cp2cr_name(27, gc_dqa).
cp2cr_name(28, gc_dqb).
cp2cr_name(31, gc_flag).

cp2dr_name( 0, gd_vxy0).
cp2dr_name( 1, gd_vz0).
cp2dr_name( 2, gd_vxy1).
cp2dr_name( 3, gd_vz1).
cp2dr_name( 4, gd_vxy2).
cp2dr_name( 5, gd_vz2).
cp2dr_name( 6, gd_rgb).
cp2dr_name( 7, gd_vz2).
cp2dr_name( 8, gd_ir0).
cp2dr_name( 9, gd_ir1).
cp2dr_name(10, gd_ir2).
cp2dr_name(11, gd_ir3).
cp2dr_name(12, gd_sxy0).
cp2dr_name(13, gd_sxy1).
cp2dr_name(14, gd_sxy2).
cp2dr_name(15, gd_sxyp).
cp2dr_name(16, gd_sz0).
cp2dr_name(17, gd_sz1).
cp2dr_name(18, gd_sz2).
cp2dr_name(19, gd_sz3).
cp2dr_name(20, gd_rgb0).
cp2dr_name(21, gd_rgb1).
cp2dr_name(22, gd_rgb2).
cp2dr_name(23, gd_res1).
cp2dr_name(24, gd_mac0).
cp2dr_name(25, gd_mac1).
cp2dr_name(26, gd_mac2).
cp2dr_name(27, gd_mac3).
cp2dr_name(28, gd_irgb).
cp2dr_name(29, gd_orgb).
cp2dr_name(30, gd_lzcs).
cp2dr_name(31, gd_lzcr).

/*
Instruction formats.
*/

:- discontiguous instruction_i/5.
:- discontiguous instruction_j/3.
:- discontiguous instruction_r/7.

instruction_r(0b000000, 0, 0, 0, 0, 0, nop).
instruction_r(0b000000, _, Rt, Rd, Sh, 0, sll(r(Rd), r(Rt), Sh)).
instruction_r(0b000000, _, Rt, Rd, Sh, 2, srl(r(Rd), r(Rt), Sh)).
instruction_r(0b000000, _, Rt, Rd, Sh, 3, sra(r(Rd), r(Rt), Sh)).
instruction_r(0b000000, Rs, Rt, Rd, 0, 4, sll(r(Rd), r(Rt), r(Rs))). % sllv
instruction_r(0b000000, Rs, Rt, Rd, 0, 6, srl(r(Rd), r(Rt), r(Rs))). % srlv
instruction_r(0b000000, Rs, Rt, Rd, 0, 7, sra(r(Rd), r(Rt), r(Rs))). % srav
instruction_r(0b000000, Rs, Rt, Rd, 0, 32, add(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 33, addu(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 34, sub(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 35, subu(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 36, and(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 37, or(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 38, xor(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 39, nor(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 42, slt(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, Rt, Rd, 0, 43, sltu(r(Rd), r(Rs), r(Rt))).
instruction_r(0b000000, Rs, 0, 0, 0, 8, jr(r(Rs))).
instruction_r(0b000000, Rs, 0, Rd, 0, 9, jalr(r(Rd), r(Rs))).

instruction_i(0b000001, Rs, 0, Im, bltz(r(Rs), Ofs)) :- im16_ofs(Im, Ofs).
instruction_i(0b000001, Rs, 1, Im, bgez(r(Rs), Ofs)) :- im16_ofs(Im, Ofs).
instruction_j(0b000010, Ix, j(pc28(Ofs))) :- Ofs #= Ix << 2.
instruction_j(0b000011, Ix, jal(pc28(Ofs))) :- Ofs #= Ix << 2.
instruction_i(0b000100, Rs, Rt, Im, beq(r(Rs), r(Rt), Ofs)) :- im16_ofs(Im, Ofs).
instruction_i(0b000101, Rs, Rt, Im, bne(r(Rs), r(Rt), Ofs)) :- im16_ofs(Im, Ofs).
instruction_i(0b000110, Rs, 0, Im, blez(r(Rs), Ofs)) :- im16_ofs(Im, Ofs).
instruction_i(0b000111, Rs, 0, Im, bgtz(r(Rs), Ofs)) :- im16_ofs(Im, Ofs).
instruction_i(0b001000, Rs, Rt, Im, add(r(Rt), r(Rs), Xi)) :- sint_integer(16, Im, Xi). % addi
instruction_i(0b001001, Rs, Rt, Im, addu(r(Rt), r(Rs), Xi)) :- sint_integer(16, Im, Xi). % addiu
instruction_i(0b001010, Rs, Rt, Im, slt(r(Rt), r(Rs), Xi)) :- sint_integer(16, Im, Xi). % slti
instruction_i(0b001011, Rs, Rt, Im, sltu(r(Rt), r(Rs), Xi)) :- sint_integer(16, Im, Xi). % sltiu
instruction_i(0b001100, Rs, Rt, Im, and(r(Rt), r(Rs), Im)). % andi
instruction_i(0b001101, Rs, Rt, Im, or(r(Rt), r(Rs), Im)). % ori
instruction_i(0b001110, Rs, Rt, Im, xor(r(Rt), r(Rs), Im)). % xori
instruction_i(0b001111, 0, Rt, Im, mov(r(Rt), Hu)) :- Hu #= Im << 16. % lui

instruction_j(0b010010, 0x2280030, rtpt).
instruction_j(0b010010, 0x2180001, rtps).
instruction_j(0b010010, 0x241e012, mvmva_rtir0).
instruction_j(0b010010, 0x2480012, mvmva_rtv0tr).
instruction_j(0b010010, 0x2486012, mvmva_rtv0).
instruction_j(0b010010, 0x2488012, mvmva_rtv1tr).
instruction_j(0b010010, 0x248e012, mvmva_rtv1).
instruction_j(0b010010, 0x2490012, mvmva_rtv2tr).
instruction_j(0b010010, 0x2496012, mvmva_rtv2).
instruction_j(0b010010, 0x2498012, mvmva_rtirtr).
instruction_j(0b010010, 0x249e012, mvmva_rtir12).
instruction_j(0b010010, 0x308041b, nccs).
instruction_j(0b010010, 0x3400006, nclip).
instruction_j(0b010010, 0x358002d, avsz3).
instruction_j(0b010010, 0x368002e, avsz4).
instruction_j(0b010010, 0x378000c, mvmva_op12).

/*
Fucking stupid horrible confusing names.
The first word isn't a verb.
The "C" doesn't stand for "Copy".
The "M" doesn't stand for "Move".
*/
instruction_r(0b010010, 0b00000, Rt, Rd, _, _, mov(r(Rt), cp2dr(Rd))). % mfc2
instruction_r(0b010010, 0b00010, Rt, Rd, _, _, mov(r(Rt), cp2cr(Rd))). % cfc2
instruction_r(0b010010, 0b00100, Rt, Rd, _, _, mov(cp2dr(Rd), r(Rt))). % mtc2
instruction_r(0b010010, 0b00110, Rt, Rd, _, _, mov(cp2cr(Rd), r(Rt))). % ctc2

instruction_i(0b100000, Rs, Rt, Im, movsx(r(Rt), m1(r(Rs) + Xi))) :- sint_integer(16, Im, Xi). % lb
instruction_i(0b100001, Rs, Rt, Im, movsx(r(Rt), m2(r(Rs) + Xi))) :- sint_integer(16, Im, Xi). % lh
% ??? instruction_i(0b100010, Rs, Rt, Im, movsx(r(Rt), m2(r(Rs) + Xi))) :- sint_integer(16, Im, Xi). % lwl
% mnemonic_op_format(lwl, 34, i(any, any, any)).
% mnemonic_op_format(lw, 35, i(any, any, any)).
instruction_i(0b100100, Rs, Rt, Im, movzx(r(Rt), m1(r(Rs) + Xi))) :- sint_integer(16, Im, Xi). % lbu
instruction_i(0b100101, Rs, Rt, Im, movzx(r(Rt), m2(r(Rs) + Xi))) :- sint_integer(16, Im, Xi). % lhu
% mnemonic_op_format(lhu, 37, i(any, any, any)).
% mnemonic_op_format(lwr, 38, i(any, any, any)).
instruction_i(0b101000, Rs, Rt, Im, mov(m1(r(Rs) + Xi), r(Rt))) :- sint_integer(16, Im, Xi). % sb
instruction_i(0b100011, Rs, Rt, Im, mov(r(Rt), m(r(Rs) + Xi))) :- sint_integer(16, Im, Xi). % lw
instruction_i(0b110010, Rs, Rt, Im, mov(cp2dr(Rt), m(r(Rs) + Im))). % lwc2
instruction_i(0b101001, Rs, Rt, Im, mov(m2(r(Rs) + Xi), r(Rt))) :- sint_integer(16, Im, Xi). % sh
instruction_i(0b111010, Rs, Rt, Im, mov(m(r(Rs) + Im), cp2dr(Rt))). % swc2
instruction_i(0b101011, Rs, Rt, Im, mov(m(r(Rs) + Im), r(Rt))). % sw
% mnemonic_op_format(swl, 42, i(any, any, any)).
% mnemonic_op_format(swr, 46, i(any, any, any)).

word_instruction_0(Word, Instruction) :-
    split_bits(Word, 26, Op, Param),
    param_r(Param, Rs, Rt, Rd, Sh, Fu),
    instruction_r(Op, Rs, Rt, Rd, Sh, Fu, Instruction).

word_instruction_0(Word, Instruction) :-
    split_bits(Word, 26, Op, Param),
    param_i(Param, Rs, Rt, Im),
    instruction_i(Op, Rs, Rt, Im, Instruction).

word_instruction_0(Word, Instruction) :-
    split_bits(Word, 26, Op, Param),
    param_j(Param, Ix),
    instruction_j(Op, Ix, Instruction).

word_instruction(Word, Instruction) :- word_instruction_0(Word, Instruction), !.
word_instruction(Word, error_unknown_instruction(Word)) :- !.

/*
split_bits(Whole, Index, Left, Right).

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

% Internal.
param_r(P, Rs, Rt, Rd, Shamt, Funct) :-
    split_bits(P , 6, P1, Funct),
    split_bits(P1, 5, P2, Shamt),
    split_bits(P2, 5, P3, Rd),
    split_bits(P3, 5, P4, Rt),
    split_bits(P4, 5, 0, Rs).

% Internal.
param_i(P, Rs, Rt, Im) :-
    split_bits(P , 16, P1, Im),
    split_bits(P1, 5, P2, Rt),
    split_bits(P2, 5, 0, Rs).

% Internal.
param_j(P, Ix) :-
    split_bits(P , 26, 0, Ix).

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
Decompiling from instructions to abstract-procedural-language statements.

We assume these to simplify the decompiler:

Branch delay slot does not contain branch-delay-slot-producing instruction.

Loads and stores finish immediately; no load delay slot.
Memory and coprocessor access are instantaneous.

https://github.com/aquynh/capstone/issues/209
"Processor-operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is Placed in the delay slot of a branch or jump."
*/

/*
instruction_statements_0/3
instruction_statements_0(Instruction, Now, Later) describes the effects of an instruction.
PC in Now is the address of the instruction itself, not added by four.

A Statement is an abstract-procedural-language statement.
*/
instruction_statements_0(nop, [], []).
instruction_statements_0(sll(D, S, T), [D := sll(S, T)], []).
instruction_statements_0(srl(D, S, T), [D := srl(S, T)], []).
instruction_statements_0(sra(D, S, T), [D := sra(S, T)], []).
instruction_statements_0(slt(D, S, T), [D := signed(S) < signed(T)], []).
instruction_statements_0(sltu(D, S, T), [D := unsigned(S) < unsigned(T)], []).
instruction_statements_0(addu(D, S, T), [D := S + T], []).
instruction_statements_0(subu(D, S, T), [D := S - T], []).
instruction_statements_0(and(D, S, T), [D := S /\ T], []).
instruction_statements_0(or(D, S, T), [D := S \/ T], []).
instruction_statements_0(xor(D, S, T), [D := S xor T], []).
instruction_statements_0(mov(D, S), [D := S], []).
instruction_statements_0(movsx(D, S), [D := sx(S)], []).
instruction_statements_0(movzx(D, S), [D := zx(S)], []).
% instruction_statements_0(i(lui, _, Rt, Im), [Rt := Val, []]) :- Val #= Im << 16.
% instruction_statements_0(i(sltiu, Rs, Rt, Im), [Rt := Rs < Val, []]) :- sint_integer(16, Im, Val).
instruction_statements_0(j(pc28(Ofs)), [], [goto((pc /\ 0xf0000000) \/ Ofs)]).
instruction_statements_0(jr(T), [jr_target := T], [goto(jr_target)]).
instruction_statements_0(jal(pc28(Ofs)), [ra := pc + 8], [goto((pc /\ 0xf0000000) \/ Ofs)]).
instruction_statements_0(jalr(D, T), [D := pc + 8], [goto(T)]).
instruction_statements_0(beq(S, T, R), [condition := (S = T)], [if(condition, pc + R, pc + 4)]).
instruction_statements_0(bne(S, T, R), [condition := (S \= T)], [if(condition, pc + R, pc + 4)]).
instruction_statements_0(bltz(S, R), [condition := (signed(S) < 0)], [if(condition, pc + R, pc + 4)]).
instruction_statements_0(bgtz(S, R), [condition := (signed(S) > 0)], [if(condition, pc + R, pc + 4)]).
instruction_statements_0(blez(S, R), [condition := (S = 0)], [if(condition, pc + R, pc + 4)]).
instruction_statements_0(rtpt, [rtpt], []).
instruction_statements_0(rtps, [rtps], []).
instruction_statements_0(nccs, [nccs], []).
instruction_statements_0(nclip, [nclip], []).
instruction_statements_0(avsz3, [avsz3], []).
instruction_statements_0(avsz4, [avsz4], []).
instruction_statements_0(mvmva_rtv0, [mvmva_rtv0], []).
instruction_statements_0(mvmva_rtv1, [mvmva_rtv1], []).
instruction_statements_0(mvmva_rtv2, [mvmva_rtv2], []).
instruction_statements_0(mvmva_op12, [mvmva_op12], []).
instruction_statements_0(mvmva_rtir12, [mvmva_rtir12], []).

instruction_statements(Ins, Now, Later) :- instruction_statements_0(Ins, Now, Later), !.
instruction_statements(Ins, [error(Message)], []) :- format(atom(Message), 'instruction_statements: ~p', [Ins]), !.

% Translate 16-bit instruction immediate to PC offset.
im16_ofs(Ins_count, Ofs) :-
    Byte_count #= Ins_count << 2,
    sint_integer(18, Byte_count, Ofs).

operand_friendly_0(I, I) :- integer(I).
operand_friendly_0(r(I), N) :- gpr_name(I, N).
operand_friendly_0(cp2cr(I), N) :- cp2cr_name(I, N).
operand_friendly_0(cp2dr(I), N) :- cp2dr_name(I, N).
operand_friendly_0(m(B+D), m(FB+FD)) :- operand_friendly(B, FB), operand_friendly(D, FD).
operand_friendly_0(m1(B+D), m1(FB+FD)) :- operand_friendly(B, FB), operand_friendly(D, FD).
operand_friendly_0(m2(B+D), m2(FB+FD)) :- operand_friendly(B, FB), operand_friendly(D, FD).
operand_friendly_0(pc28(A), pc28(FA)) :- operand_friendly(A, FA).
operand_friendly_0(zx(A), zx(FA)) :- operand_friendly(A, FA).
operand_friendly_0(sx(A), sx(FA)) :- operand_friendly(A, FA).
operand_friendly_0(signed(A), signed(FA)) :- operand_friendly(A, FA).
operand_friendly_0(unsigned(A), unsigned(FA)) :- operand_friendly(A, FA).

operand_friendly(A, B) :- operand_friendly_0(A, B), !.
operand_friendly(A, A) :- !.
