% -------------------- parsing with constraints and without lists

:- use_module(library(tabling)).

%'$input'("0+1").
'$input'("00000000000000000000000000000000000000000000000000+11111111111111111111111111111111111111111111111111").

'$input_length'(N) :- '$input'(String), string_length(String, N).

'$position_code'(P, C) :-
    '$input'(String),
    string_codes(String, Codes),
    nth0(P, Codes, C).

% -------------------- macro

%%  position_code(?Position, ?Code) is nondet.
:-  findall(position_code(P,C), '$position_code'(P,C), Facts),
    compile_aux_clauses(Facts).

%%  input_length(?Length) is det.
:-  '$input_length'(N),
    compile_aux_clauses([input_length(N)]).

input_index1(A) :- input_length(N), between(1,N,A).

% xyz(P,Q) means there is an xyz from position P inclusive to position Q exclusive.

:- op(1190,xfx,=>).

% This terminates, but is still too slow.

%:- table number/2.
%:- table exp/2.

% Rule(BeginInclusive,EndExclusive)

code(C,P,Q) :- position_code(P,C), Q is P+1.

digit(P,Q) :- code(0'0,P,Q).
digit(P,Q) :- code(0'1,P,Q).
op(P,Q) :- code(0'+,P,Q).
number(P,Q) :- digit(P,Q).
number(P,R) :- digit(P,Q), number(Q,R).

:- dynamic cache/2.

exp(P,Q) :- cache(exp(P,Q),false), !, false.
exp(P,Q) :- number(P,Q), assert(cache(exp(P,Q),true)).
exp(A,Z) :- input_length(N), between(1,N,A), between(A,N,Z), A < Z, exp(A,B), op(B,C), exp(C,Z).
