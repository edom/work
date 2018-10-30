---
title: Relation
mathjax: yes
date: 2017-06-29 22:40 +0700
permalink: /relation.html
---

# Relation

The *arity* of a relation is its number of parameters.
For example, if \\(A\\) has arity 2, then \\(A(x,y)\\) is a *formula*.

Relations of arity 0, 1, and 2 are also called *nullary*, *unary*, and *binary*, respectively.

A *constant* is a nullary relation.

A *set* is a unary relation.
We can write either \\(A(x)\\) or \\(x \in A\\) to mean that \\(x\\) is an _element_ of the set \\(A\\).

What is the difference between a relation and a predicate?

A pedantic note:
Theoretically, a formula is not a truth value,
and it is the *interpretation* that maps formulas to truth values.
For example, if \\(A\\) is a unary relation,
then \\(A(x)\\) is a formula, not the truth value,
and therefore it does not make sense to say "\\(A(x)\\) is true".
Practically, the interpretation is implicit, and we say "\\(A(x)\\) is true" to mean "the implied interpretation maps \\(A(x)\\) to truth value 1".

## Binary relations

These are isomorphic:

- [binary relation](https://en.wikipedia.org/wiki/Binary_relation),
- [transition system](https://en.wikipedia.org/wiki/Transition_system),
- [rewriting system](https://en.wikipedia.org/wiki/Abstract_rewriting_system),
- [directed graph](https://en.wikipedia.org/wiki/Directed_graph) (digraph).

Every binary relation \\(A\\) is also a _directed graph_
where \\((x,y)\\) is an edge iff \\(A(x,y)\\).
We will mix terms.
For example, a relation is _acyclic_ iff its graph is acyclic.

Let \\(A\\) be a binary relation.

Iff \\(A(x,y)\\), then \\(x\\) is in the _domain_ of \\(A\\).

Iff \\(A(x,y)\\), then \\(y\\) is in the _range_ of \\(A\\).

\\(x\\) _reaches_ \\(y\\) (\\(y\\) is _reachable_ from \\(x\\))
iff there is a path from \\(x\\) to \\(y\\).

\\(x\\) is _initial_ iff its in-degree is zero (no \\(y\\) satisfying \\(A(y,x)\\)).

\\(x\\) is _terminal_ iff its out-degree is zero (no \\(y\\) satisfying \\(A(x,y)\\)).

The *composition* of \\(A\\) and \\(B\\) is \\(A \circ B\\)
where <span>\( (A \circ B)(x,y) = \exists m ( B(x,m) \wedge A(m,y) ) \)</span>.
Note the swap: first \\( B \\) maps \\(x\\) to \\(m\\),
and then \\( A \\) maps \\(m\\) to \\(y\\).
Function composition is special case of relation composition.

The *\\(n\\)th self-composition* of \\(A\\) is <span>\( A^n = A \circ A^{n-1} \)</span>.

The *infinite self-composition* of \\(A\\) is <span>\( A^\infty \)</span>,
the smallest relation satisfying \\( A \circ A^\infty = A^\infty \\).
We also say that \\(A^\infty\\) is the *least fixed point* of \\( F \\) where <span>\( F(X) = A \circ X \)</span>.

The *transitive closure* of \\(A\\) is the union of all self-compositions of \\(A\\).
Formally, <span>\( tc(A) = \bigcup_{k=1}^\infty A^k \)</span>.

## External links

- [ProofWiki definition of relation](https://proofwiki.org/wiki/Definition:Relation)
