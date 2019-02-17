:- module(language_prolog_callgraph, [
    kb_caller_callee/3,
    kb_calledges/2,
    kb_callgraphtc/2,
    kb_depend/3,
    ugraph_edge/3
]).
:- use_module(library(ugraphs)).
:- use_module('./language_prolog_kb.pro').

/** kb_caller_callee(+Rules, ?Caller, ?Callee)

True iff Caller may directly call Callee in Rules.

Caller is a functor that is unified with a copy of a Horn head in Rules.
Caller is not a predicate specification (Name/Arity).

This can be used to build the call graph.
*/
kb_caller_callee(G,C,D) :-
    kb_rule(G,(C:-B)),
    clause_phrase(B,D),
    callable(D).

    clause_phrase((A,_),B) :- clause_phrase(A,B).
    clause_phrase((_,A),B) :- clause_phrase(A,B).
    clause_phrase((A;_),B) :- clause_phrase(A,B).
    clause_phrase((_;A),B) :- clause_phrase(A,B).
    clause_phrase(A,B) :- A \= (_,_), A \= (_;_), A = B.

kb_calledges(K,E) :- setof(A-B, kb_caller_callee(K,A,B), E), !.
kb_calledges(_,[]) :- !.

kb_callgraphtc(K,Closure) :-
    kb_calledges(K,Edges),
    vertices_edges_to_ugraph([],Edges,Graph),
    transitive_closure(Graph, Closure).

/** kb_depend(+Rules,+X,+Y)

True iff X may call Y directly or indirectly.

All arguments must be bound; otherwise the predicate may not terminate.
*/
kb_depend(G,A,B) :- kb_caller_callee(G,A,B).
kb_depend(G,A,C) :- kb_caller_callee(G,A,B), kb_depend(G,B,C).

ugraph_edge(G,V,W) :- neighbors(V,G,N), member(W,N).
