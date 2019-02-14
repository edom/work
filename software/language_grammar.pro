:- module(language_grammar, [
    grammar_kb/2
]).
:- use_module('./language_prolog.pro').
/** <module> grammar description language

A language that describes non-left-recursive context-free languages.

This does not work with left-recursive grammars.
*/

/** grammar_kb(+Grammar, -Rules)

Translate grammar description to difference-list knowledge base.

Similar to definite-clause grammar (DCG).
*/
grammar_kb([],[]) :- !.
grammar_kb([GA|GB],P) :- !, P=[PA|PB], grammar_kb(GA,PA), grammar_kb(GB,PB).
grammar_kb((GHead:-GBody), (PHead:-PBody)) :-
    ghead_phead(GHead,PHead,Input,Rest),
    gbody_pbody(GBody,PBody,Input,Rest).

    ghead_phead(G,P,I,R) :- functor_addargs(G,[I,R],P).

        functor_addargs(F0,Args,F1) :- F0=..F, append(F,Args,G), F1=..G.

    gbody_pbody(#G,P,_,_) :- !, P = #G.
    gbody_pbody((GA,GB),P,S0,S9) :- !, P=(PA,PB), gbody_pbody(GA,PA,S0,S4), gbody_pbody(GB,PB,S4,S9).
    gbody_pbody((GA;GB),P,S0,S9) :- !, P=(PA;PB), gbody_pbody(GA,PA,S0,S9), gbody_pbody(GB,PB,S0,S9).
    gbody_pbody((GA|GB),P,S0,S9) :- !, P=(PA;PB), gbody_pbody(GA,PA,S0,S9), gbody_pbody(GB,PB,S0,S9).
    gbody_pbody(G,P,S0,S9) :- string(G), !, string_codes(G,Codes), P = #append(Codes,S9,S0).
    gbody_pbody(G,P,S0,S9) :- functor_addargs(G,[S0,S9],P).
