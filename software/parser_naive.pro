/*
The language parsed by expression/1 is:

E ::= a | E + E

string_codes("a+a", S), expression(S).
*/

plus([0'+]).

expression([0'a]).

/*
This straightforward translation doesn't work.
Left recursion.

expression(E) :- append([A,B,C], E),
    expression(A), plus(B), expression(C).
*/

% If the length of E isn't ground, this fails.
expression(E) :-
    plus(B), append([A,B,C], E),
    expression(A), expression(C).
