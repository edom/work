/** <module> abstract procedural language

*/

:- module(ps1_procedural, [
    range_blocks/2
    , expression_simplified/2
    , statement_branch_target/2
    , instruction_statements/3
]).

:- use_module(library(clpfd)).
:- use_module('./map.pro').
:- use_module('./ps1_cpu.pro').
:- use_module('./ps1_memory.pro').

/** range_blocks(Range, Blocks)

"The memory region Range consists of the basic blocks Blocks."

Range has the shape Begin-End.

Blocks is a list of Block.
    - A Block has the shape Label : Statements.
        - Label is an integer.
        - Statements is a list of Statement.
            - A Statement is defined in instruction_statements/3.

Statements represents the effect of executing the instruction from Begin inclusive to End exclusive.

Notes on branch delay slots:

The instruction sequence

    A: BRANCH somewhere
    B: SLOT
    C: ...

roughly translates to the statement sequence

    A : [ slot, branch, goto(C) ]
    B : [ slot ]
    C : ...
*/
range_blocks(Begin-End, []) :- Begin #>= End, !.

range_blocks(Begin-End, [Begin : BranchAndSlot, SlotBegin : Slot | Rest]) :-
    address_instruction_friendly(Begin, Instruction),
    instruction_statements(Instruction, Now, Later),
    Later \= [], !, % a branch instruction
    SlotBegin #= Begin + 4,
    SlotEnd #= SlotBegin + 4,
    SlotEnd #=< End,
    substitute(pc, Begin, Now, Now0),
    substitute(pc, SlotBegin, Later, Later0),
    range_blocks(SlotBegin-SlotEnd, [_ : Slot]),
    append(Slot0, [goto(_)], Slot), !, % Why does this create a choice point?
    append([Now0, Slot0, Later0, [goto(SlotBegin + 4)]], Branch_and_slot_0),
    statements_fix(Branch_and_slot_0, BranchAndSlot),
    Next #= Begin + 8,
    range_blocks(Next-End, Rest).

range_blocks(Begin-End, [Begin : Simple | Rest]) :-
    address_instruction_friendly(Begin, Instruction),
    instruction_statements(Instruction, Simple_0, []),
    % This ensures that every block has a branch target, for statement_branch_target/2.
    append(Simple_0, [goto(Begin + 4)], Simple_1),
    statements_fix(Simple_1, Simple),
    !,
    Next #= Begin + 4,
    range_blocks(Next-End, Rest).

range_blocks(Begin-End, [Begin : [error(Message)] | Rest]) :-
    address_word(Begin, Word),
    format(atom(Message), 'range_blocks: no instruction_statements/3 for 0x~16r at address 0x~16r', [Word, Begin]),
    Next #= Begin + 4,
    range_blocks(Next-End, Rest).

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

% statement_branch_target(Statement, Target) means Statement indicates a possible branch to Target.
statement_branch_target(ra := B, B).
statement_branch_target(goto(B), B).
statement_branch_target(if(_, B, _), B).
statement_branch_target(if(_, _, B), B).

address_instruction_friendly(Address, Instruction) :-
    address_instruction(Address, I),
    instruction_friendly(I, Instruction).

/*
instruction_statements_0/3
instruction_statements_0(Instruction, Now, Later) describes the effects of an instruction.
PC in Now is the address of the instruction itself, not added by four.

A Statement is an abstract-procedural-language statement.

Here we decompile an instruction to its corresponding higher-level statements.

We assume these to simplify the decompiler:
- Branch delay slot does not contain branch-delay-slot-producing instruction.
- Loads and stores finish immediately; no load delay slot.
  Memory and coprocessor access are instantaneous.

https://github.com/aquynh/capstone/issues/209
"Processor-operation is UNPREDICTABLE if a branch, jump, ERET, DERET, or WAIT instruction is Placed in the delay slot of a branch or jump."
*/
instruction_statements_0(nop, [], []).
instruction_statements_0(sll(D, S, T), [D := S << T], []).
instruction_statements_0(srl(D, S, T), [D := unsigned(S) >> T], []).
instruction_statements_0(sra(D, S, T), [D := signed(S) >> T], []).
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

/** instruction_statements(Ins, Now, Later).

"Instruction Ins translates to the statements Now and Later.

Ins should come from the second argument of instruction_friendly/2.
(TODO Should we wire this to word_instruction/2 instead?)

Now executes before the next instruction execute.

Later executes after the next instruction executes.

If Later is not empty, then Ins is a branch instruction.

We only model as far as necessary for decompilation.
We don't model the entire processor pipeline.
*/
instruction_statements(Ins, Now, Later) :- instruction_statements_0(Ins, Now, Later), !.
instruction_statements(Ins, [error(Message)], []) :- format(atom(Message), 'instruction_statements: ~p', [Ins]), !.
