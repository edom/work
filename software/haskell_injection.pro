:- module(haskell_injection, [
    injection/4
]).
:- use_module('./haskell.pro', []).

/** injection(?Typ, ?ExtTyp, ?Val, ?ExtVal)

"Every Val which is an inhabitant of Typ can be injected to ExtVal which is an inhabitant of ExtTyp."

Example:
injection(T, maybe(T), V, just(V))
means that the =just= constructor injects every inhabitant of T to maybe(T).

*/
injection(T, maybe(T), V, just(V)).
injection(A, either(A,_), V, left(V)).
injection(B, either(_,B), V, right(V)).

haskell:type_value(T,V) :- type_value(T,V).

type_value(integer, V) :- integer(V).
type_value(maybe(_), nothing).
type_value(maybe(T), just(V)) :- type_value(T, V).
type_value(either(A,_), left(V)) :- type_value(A, V).
type_value(either(_,B), right(V)) :- type_value(B, V).
