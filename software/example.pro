:- use_module('data.pro').
:- use_module(library(clpfd)).

entity(todo, [
        [id, int32]
        , [text, varchar(1024)]
        , [what, int32, nullable]
    ]).

main(Class, Ast) :- java_class_ast(Class, Ast).

:- use_module('java_syntax.pro').

test_java_syntax(Tree) :- true
    , string_codes("1 + 2", Source)
    , phrase(expression(Tree), Source, [])
    .


bit(0).
bit(1).

bfs([]).
bfs([H|T]) :- bfs(T), bit(H).

dfs([]).
dfs([H|T]) :- bit(H), dfs(T).

/*
Prolog can handle left-recursive grammar if we ground terms as much as possible before recursing.
*/

% S ::= <empty> | S a

word([]).
word(A) :- append(B, [a], A), word(B).

% S ::= <empty> | S T S
% T ::= a

word2([]).
word2(S) :- t(T), append([S0,T,S1], S), word2(S0), word2(S1).

t([a]).
