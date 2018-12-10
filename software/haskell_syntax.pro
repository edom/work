:- module(haskell_syntax, [
]).
:- use_module(library(dcg/basics)).
:- use_module('./haskell.pro').

% tokens

white([C|Cs]) --> [C], {code_type(C, white)}, (white(Cs) ; {Cs=[]}).
tokens([T|Ts]) --> token(T), (tokens(Ts) ; {Ts=[]}).
token(digits(Ds)) --> digits(Ds).
token(white(Cs)) --> white(Cs).
token(valname(Cs)) --> valname(Cs).
token(lparen) --> "(".
token(rparen) --> ")".
valname([C|Cs]) --> [C], {code_type(C,lower)}, nonblanks(Cs).

% concrete syntax tree

exp(con('Integer',V)) --> integer(V).
exp(app(TA -> TB,A,B)) :- exp(A), exp(B).
