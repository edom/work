---
title: Theory of deciders
language: en
mathjax: yes
---

We explore the theory of *deciders*
(which are just unary algebras).
We try to describe computation without being tied to a machine model.
We hope we can lower bound and upper bound of computation complexity.

There are two ways to define a *decider*:
the algebraic way and the logic way.
The logic way naturally gives rise to graph.
However, the following is the algebraic definition.

\(\newcommand\calR{\mathcal{R}}
\newcommand\Bool{\mathbb{B}}
\newcommand\fcost{\textsf{cost}}
\newcommand\fspace{\textsf{space}}
\newcommand\ftt{\textsf{tt}}
\newcommand\ftime{\textsf{time}}
\newcommand\calD{\mathcal{D}}\)A *decider* \(\calD\) is an algebra \((S,f,0,1)\) satisfying these:
\[
\begin{align*}
f~0 = 0
\\
f~1 = 1
\end{align*}
\]
where the set \(S\) is the *underlying set*,
the function \(f\) is the *transition function*,
the element \(0\) is the *rejecting state*,
and the element \(1\) is the *accepting state*.
A *state* is an element of the underlying set.
The accepting and rejecting state are collectively known as *terminal* states.

Deciders and predicates are closely related:
a decider with transition function \(f\) *computes* a predicate \(c~f\)
satisfying these:
\[
\begin{align*}
c~f \circ f &= c~f
\\ f \circ c~f &= c~f
\end{align*}
\]
where
\(c : (S \to S) \to (S \to \{0,1\})\)
is the *short-circuiting function*.
The function \(c~f\) is also called the short-circuited \(f\).

We want to *compose*, build deciders from deciders.
Why?
Finite primitives?

\((A,f,0,1)\) becomes \((A, \neg f,1,0)\).

\((A,f)\) and \((B,g)\) becomes \((2 \times A \times B, f \wedge g)\).
Let \(h = f \wedge g\).
\[
\begin{align*}
h~0 = 0
\\ h~1 = 1
\\ h~1 = 1
\end{align*}
\]

\(A\)-deciders form a *Boolean algebra*,
But why should we bother showing this?
\((\neg f)~x = \neg~(f~x)\).
\((f \wedge g)~x = f~x \wedge g~x\).
\((f \vee g)~x = f~x \vee g~x\).

A decider is too general,
so we want to restrict it.

The questions are:

* Does a restriction affect what the decider can compute?
* Does a restriction affect the running time of the decider for a given input?

\(\newcommand\Nat{\mathbb{N}}\)We can *constrain the transition function* by *distance constraining*.
We assume the existence of a *distance function* \(d : V \to V \to \Nat\)
such that \(f\) has to satisfy \(d~x~(f~x) \le k\) where \(k\) is a constant, usually 1.
The structure \((S,d)\) then becomes a *metric space*.
The distance function must satisfy:
\[
\begin{align*}
d~x~x &= 0
\\ d~x~y &= d~y~x
\end{align*}
\]

We can *constrain the transition function* by *vertex label constraining*.
Let there be a vertex labeling \(m : V \to \Nat\).
(We are silently assuming that \(V\) is countable.)
The labeling \(m\) is surjective but not necessarily injective.
This labeling divides the graph into maximal subconnected subcomponents?
The constraints are:

* Each label is used finitely many times.
There are only finitely many vertices with the same label.
* The function \(f\) must satisfy \(|m~(f~x) - m~x| \le d\)
where \(d\) is a finite number, usually 1.

## What class does this constrained decider correspond to?

We can also *constrain the state size*.

We posit the existence of a *state size function* \(s : A \to \Nat\) where:

* There are exactly \(2^n\) states of size \(n\).
* If \(s~x \neq s~y\) then \(x \neq y\).

\[
\forall n \in \Nat: |\{ x ~|~ s~x = n\} = 2^n|
\]

We then further constrain the transition function \(f\) such that \(|s~(f~x) - s~x| \le 1\).

We can constrain the transition function \(f\) such that \(s~x - s~(f~x) = -1\).

An alternative to this is defining fringe functions \(A \to P~A\),
and work with the generated \((P~A, F)\).

# Finite

A decider is *finite* iff its underlying set is finite.

If \(S\) is finite then \(S \to S\) is also finite.

# Relationship with predicates

The set of all \(A\)-deciders is isomorphic to the set of all *\(A\)-predicates*.
If \(|A| = n\) then there are \(2^n\) different \(A\)-predicates (extensionally).

A *total* decider has total \(c~f\).
A *partial* decider has partial \(c~f\).

A state \(s\) is *eventually terminal* iff \(c~f~s\) is defined,
is *eventually rejecting* iff \(c~f~s = 0\),
and is *eventually accepting* iff \(c~f~s = 1\).

The *generating set* \(G\) is the set of all \(x\)
that is not in the range of \(f\).
\[
\begin{align*}
G = A - \{ f~x | x \in A \}
\end{align*}
\]

The generating set is the set of *initial states*.

A state is *initial* iff it is in \(G\).

# Logic calculus decider

Let each vertex represent a logic expression.

Let an edge from \(x\) to \(y\) mean that there is
an inference rule that step-reduces the expression \(x\) to \(y\).

# Decider

## Logical definition

A *decider* is \((V,E,A)\) with signature \((2,1,1)\).
A decider is a directed graph with some special vertexes.
A *state* is a vertex.

\(A~x\) is true iff \(x\) is an *accepting* vertex.

\(E~x~y\) is true iff \(y\) is a successor of \(x\).
The relation \(E~x~y\) is true iff there is an edge from \(x\) to \(y\).

The decider is *deterministic* iff \(E\) is injective:
\[
E~x~y \wedge E~x~z \implies y = z
\]

\(E\) is injective iff each vertex has out-degree one or less.

### Path-existence predicate

\(P~x~y\) means there is path from \(x\) to \(y\).

\[
\begin{align*}
E~x~y &\implies P~x~y
\\ E~x~y \wedge P~y~z &\implies P~x~z
\\ \alpha~x &= P~x~y \wedge A~y
\end{align*}
\]

\[
\begin{align*}
E~x~y &\implies \neg A~x
\end{align*}
\]

\[
\begin{align*}
E~x~y &\vdash P~x~y
\\ E~x~y , P~y~z &\vdash P~x~z
\\ E~x~y , A~x &\vdash
\end{align*}
\]

Defining the predicate \(T\):

\(T~x~n\) means there exists a path of length \(n\) from \(x\) to a terminal vertex.

\[
\begin{align*}
A~x &\implies T~x~0
\\ R~x &\implies T~x~0
\\ E~x~y \wedge T~y~k &\implies T~x~(1 + k)
\end{align*}
\]

# Descriptive/algorithmic complexity

Suppose that there is a *complexity* function that maps a predicate to a complexity measure.

\(c : (A \to \Bool) \to C\).

Assume that \(c_L~p\) is the length of the shortest \(L\)-program for the predicate \(p\).
What does this even mean?

Can a function be stated as a combination of other functions?
What combination?

[Logic of graphs](https://en.wikipedia.org/wiki/Logic_of_graphs)

# Inefficientizing

Given an \(A\)-decider and infinite \(A\),
we can always construct another \(A\)-decider
that computes the same predicate but with more states.

Given a non-oracle \(A\)-decider, we can always construct another \(A\)-decider
that computes the same predicate but with less states.
