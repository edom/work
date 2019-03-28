% Non-deterministic expression language.

g(1,1).
g(2,2).
f(A,B) :- B is A + 1.

% Import predicate as function.
reduce(A + B, Z) :- number(A), number(B), Z is A + B.
reduce(A + B, Z) :- reduce(A, A0), Z = A0 + B.
reduce(A + B, Z) :- reduce(B, B0), Z = A + B0.
reduce(f(A), B) :- f(A,B).
reduce(g(A), B) :- g(A,B).
reduce(amb(List), B) :- member(B, List).

interpret(E, V) :- reduce(E, R), interpret(R, V).
interpret(E, E).

% Example: interpret(amb([1,2]) + amb([3,4]), V).
