/*
Simplification by abstract interpretation (partial evaluation).

TODO convert to SSA form first before simplifying
*/
:- module(ps1_procedural_simplify, [
    address_contains_constant/1
    , simplify_by_abstract_interpretation/4
]).

:- use_module('./map.pro').
:- use_module('./ps1_memory.pro').

/*
address_contains_constant(Address) means that we want the simplifier to replace m(Address)
with the actual corresponding value in the memory dump file.
*/
:- multifile address_contains_constant/1.
:- dynamic address_contains_constant/1.

/*
Simplify statements by simulation.

The input is Block0.
It must be a basic block.

The outputs are Known1 and Block1.

Known1 is list of bindings such as [t0 = 0, t1 = 1, t2 = m(0x1f800000)]
that we are sure of at the end of the block.
*/
simplify_by_abstract_interpretation(Known0, Block0, Known1, Block1) :-
    simulate(Known0, Block0, Known1, Block1).

simulate(Known0, [], Known0, []) :- !.
simulate(Known0, [H | T], Known2, [HH | TT]) :-
    simulate_1(Known0, H, Known1, HH),
    simulate(Known1, T, Known2, TT).

simulate_1(Known0, A := B, Known2, SA := SB) :- !,
    lvalue_simplified(Known0, A, SA),
    rvalue_simplified(Known0, B, SB),
    binding_evict(SA, Known0, Known1),
    binding_try_add(SA = SB, Known1, Known2),
    format("~p --> ~p  ;  ~p --> ~p~n", [A := B, SA := SB, Known0, Known2]),
    true.

simulate_1(Known0, goto(A), Known0, goto(SA)) :- !,
    rvalue_simplified(Known0, A, SA).

simulate_1(Known0, if(C, T, F), Known0, Out) :- !,
    rvalue_simplified(Known0, C, SC),
    lvalue_simplified(Known0, T, ST),
    rvalue_simplified(Known0, F, SF),
    branch_simplified(if(SC, ST, SF), Out).

simulate_1(_, S, [], S). % We don't know what the statement does. All bets are off.

branch_simplified(if(true, A, _), goto(A)) :- !.
branch_simplified(if(false, _, A), goto(A)) :- !.
branch_simplified(A, A).

lvalue_simplified(Known, m(A), m(B)) :- !, rvalue_simplified(Known, A, B).
lvalue_simplified(Known, m2(A), m2(B)) :- !, rvalue_simplified(Known, A, B).
lvalue_simplified(Known, m1(A), m1(B)) :- !, rvalue_simplified(Known, A, B).
lvalue_simplified(_, A, A).

addition_variables_constant(A, [], A) :- integer(A), !.
addition_variables_constant(A + B, V, C) :-
    addition_variables_constant(A, VA, CA),
    addition_variables_constant(B, VB, CB),
    append(VA, VB, V),
    C is CA + CB, !.
addition_variables_constant(A, [A], 0).

variables_constant_sum([], C, C) :- !.
variables_constant_sum(V, 0, S) :- summands_sum(V, S), !.
variables_constant_sum(V, C, S + C) :- summands_sum(V, S), !.

summands_sum([], 0) :- !.
summands_sum([A], A) :- !.
summands_sum([A|As], B + A) :- summands_sum(As, B). % left-associate

addition_simplified(A, S) :-
    addition_variables_constant(A, V, C),
    variables_constant_sum(V, C, S).
addition_simplified(A, A).

rvalue_simplified(Known, m(Exp0), Exp2) :- !, rvalue_simplified(Known, Exp0, Exp1), rload_simplified(m(Exp1), Exp2).
rvalue_simplified(Known, m2(Exp0), Exp2) :- !, rvalue_simplified(Known, Exp0, Exp1), rload_simplified(m2(Exp1), Exp2).
rvalue_simplified(Known, m1(Exp0), Exp2) :- !, rvalue_simplified(Known, Exp0, Exp1), rload_simplified(m1(Exp1), Exp2).
rvalue_simplified(Known, Exp0, C) :-
    arithmetic_expression(Exp0, Op, A, B), !,
    rvalue_simplified(Known, A, SA),
    rvalue_simplified(Known, B, SB),
    addition_simplified(SA, TA),
    addition_simplified(SB, TB),
    Exp1 =.. [Op, TA, TB],
    ((integer(TA), integer(TB)) -> C is Exp1 ; C = Exp1).
rvalue_simplified(Known, A = B, C) :-
    rvalue_simplified(Known, A, SA),
    rvalue_simplified(Known, B, SB),
    equality_simplify(SA, SB, C).
rvalue_simplified(Known, Name, Val) :- member(Name = Val, Known), !.
rvalue_simplified(_, A, A).

truefail_boolean(E, true) :- E, !.
truefail_boolean(_, false).

equality_simplify(A, A, true) :- integer(A), !.
equality_simplify(A, B, A = B).

rload_simplified(m(Address), Value) :- integer(Address), address_contains_constant(Address), address_word(Address, Value), !.
rload_simplified(m2(Address), Value) :- integer(Address), address_contains_constant(Address), address_half(Address, Value), !.
rload_simplified(m1(Address), Value) :- integer(Address), address_contains_constant(Address), address_byte(Address, Value), !.
rload_simplified(E, E).

arithmetic_expression(A + B, +, A, B).
arithmetic_expression(A - B, -, A, B).
arithmetic_expression(A \/ B, \/, A, B).
arithmetic_expression(A /\ B, /\, A, B).
arithmetic_expression(A << B, <<, A, B).
arithmetic_expression(A >> B, >>, A, B).

binding_try_add(A = B, Known0, Known1) :-
    affects(A, B), !, % TODO Static Single Assignment would obviate this check.
    binding_retract(A = _, Known0, Known1).
binding_try_add(A = B, Known0, Known1) :- !, binding_assert(A = B, Known0, Known1).

/*
binding_evict(Cause, Known0, Known1) retracts all bindings that may be affected by change in Cause.
Cause is typically a register.
*/
binding_evict(Cause, Known0, Known1) :- filter(A = _, \+ affects(Cause, A), Known0, Known1).

% Possible memory aliasing.
% If there is a write to an unknown address, invalidate all known memory locations.
% This is slightly too pessimistic.
affects(m(A), m(_)) :- \+ integer(A), !.
affects(m(A), m2(_)) :- \+ integer(A), !.
affects(m(A), m1(_)) :- \+ integer(A), !.
affects(m2(A), m(_)) :- \+ integer(A), !.
affects(m2(A), m2(_)) :- \+ integer(A), !.
affects(m2(A), m1(_)) :- \+ integer(A), !.
affects(m1(A), m(_)) :- \+ integer(A), !.
affects(m1(A), m2(_)) :- \+ integer(A), !.
affects(m1(A), m1(_)) :- \+ integer(A), !.

affects(R, R) :- !.
affects(_, []) :- !, false.
affects(R, [A | _]) :- affects(R, A), !.
affects(R, [_ | A]) :- affects(R, A), !.
affects(R, T) :-
    T =.. [F | Args],
    F \= '[|]',
    affects(R, Args).

/*
affects(R, A + _) :- affects(R, A).
affects(R, _ + A) :- affects(R, A).
affects(R, A - _) :- affects(R, A).
affects(R, _ - A) :- affects(R, A).
affects(R, A << _) :- affects(R, A).
affects(R, _ << A) :- affects(R, A).
affects(R, A >> _) :- affects(R, A).
affects(R, _ >> A) :- affects(R, A).
affects(R, A /\ _) :- affects(R, A).
affects(R, _ /\ A) :- affects(R, A).
affects(R, A \/ _) :- affects(R, A).
affects(R, _ \/ A) :- affects(R, A).
affects(R, signed(A)) :- affects(R, A).
affects(R, unsigned(A)) :- affects(R, A).
affects(R, A = _) :- affects(R, A).
affects(R, _ = A) :- affects(R, A).
affects(R, A \= _) :- affects(R, A).
affects(R, _ \= A) :- affects(R, A).
affects(R, A < _) :- affects(R, A).
affects(R, _ < A) :- affects(R, A).
affects(R, A > _) :- affects(R, A).
affects(R, _ > A) :- affects(R, A).
affects(R, A =< _) :- affects(R, A).
affects(R, _ =< A) :- affects(R, A).
affects(R, A >= _) :- affects(R, A).
affects(R, _ >= A) :- affects(R, A).
affects(R, m(A)) :- affects(R, A).
affects(R, m2(A)) :- affects(R, A).
affects(R, m1(A)) :- affects(R, A).
*/

binding_retract(_, [], []) :- !.
binding_retract(A = B, [A = B | Known0], Known0) :- !.
binding_retract(A = B, [C | Known0], [C | Known1]) :- !, binding_retract(A = B, Known0, Known1).

binding_assert(Name = Val, [], [Name = Val]) :- !.
binding_assert(Name = Val, [Name = _ | Rest], [Name = Val | Rest]) :- !.
binding_assert(Name = Val, [B | Rest0], [B | Rest1]) :- !, binding_assert(Name = Val, Rest0, Rest1).
