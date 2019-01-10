:- module(pub_bib_syntax, [
    tokens//1,
    bib//1,
    test/0
]).
:- use_module('./dcg_common.pro').

tokens([]) --> [].
tokens(T) --> spaces1(_), tokens(T).
tokens(T) --> comment(_), tokens(T).
tokens([H|T]) --> token(H), tokens(T).

comment(S) --> "%", comment_body(S).
comment_body([H]) --> [H], {H=0'\n}.
comment_body([H|T]) --> [H], {H\=0'\n}, comment_body(T).

token(at) --> "@".
token(word(Type)|T]) --> ident(Type).
token(open) --> "{".
token(close) --> "}".
token(comma) --> ",".
token(eq) --> "=".

bib([]) --> [].
bib([e(K,N,A)|T]) --> entry(K,N,A), file(T).

entry(Type,Name,Props) --> [at, word(Type), open, word(Name)], props(Props), optional([comma]), [close].

props([]) --> [].
props([p(N,V)|T]) --> prop(N,V), props(T).

prop(N,V) --> [comma, word(N), eq, word(V)].

ident(S) --> many0_reluctant(ident_char, S).
ident_char(H) --> [H], {code_type(H,csym)}.


optional(G) --> [] ; G.

many0_reluctant(F, []) --> [].
many0_reluctant(F, [H|T]) --> call(F,H), many0_reluctant(F,T).

many0_greedy(F, [H|T]) --> call(F,H), many0_greedy(F,T).
many0_greedy(F, []) --> [].

many1_greedy(F, [H|T]) --> call(F,H), many1_greedy(F,T).
many1_greedy(F, [H]) --> call(F,H).
