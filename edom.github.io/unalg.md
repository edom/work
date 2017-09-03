---
title: Unary algebra
permalink: /unalg.html
language: en
mathjax: yes
date: 2018-05-16 00:26 +0700
---

A *mono-unary algebra* \((A,f)\) is a set \(A\) and a unary function \(f : A \to A\).

There is always an injection from a unary algebra \((A, f)\)
to the *magma* \((A, F~f)\)
where \(F~f~x~y = f~x\).
The binary operation \(F~f\) ignores its second argument.
This magma happens to also be a noncommutative *semigroup*:
if we let \(g = F~f\), then \(g~(g~x~y)~z = g~x~(g~y~z)\).
Therefore the variety of unary algebras is isomorphic
to a subvariety of magmas.

There is also always an injection from a magma \((A, g)\)
to the unary algebra \((A^2, G~g)\)
where \(G~g~(x,y) = (g~x~y, ~ y)\).
The unary operation \(G~g\) passes through the second component.

If \(A\) is infinite,
there is always a bijection between \(A\) and \(A^2\).

If \(A\) and \(B\) have the same cardinality,
then for each injection \(m : A \to B\) and function \(f : A \to A\),
there is always a \(g : B \to B\) such that \(m~(f~x) = g~(m~x)\).
More understandably,
\[
f~x = y \iff g~(m~x) = m~y
\]

If \(A\) and \(B\) have the same cardinality,
then for each function \(f : A \to A\),
there is always an injection \(m : A \to B\)
and a function \(g : B \to B\) such that \(m~(f~x) = g~(m~x)\).

If \(A\) and \(B\) are isomorphic (have the same cardinality),
then the variety of \(A\)-unary algebras and the variety of \(B\)-unary algebras are isomorphic.

*The variety of \(A\)-unary algebras*
is the set of all unary algebras whose underlying set is \(A\).

**Lemma:** There is always an isomorphism between two varieties of unary algebras
whose underlying sets have the same cardinality.

**Corollary:** if \(A\) is infinite, then the variety of \(A\)-unary algebras
and the variety of \(A^2\)-unary algebras are isomorphic.

If \(A\) is infinite,
there is always a bijection \(m : A \to A^2\)
such that \(m~(f~x) = g~(m~x)\).

If \(A\) is infinite,
then there is always an injection from a unary algebra \((A^2,f)\)
to the unary algebra \((A,g)\).

If \(A\) is infinite,
then there is always a bijection between
a unary algebra \((A,f)\) and a unary algebra \((A^2,g)\),
for every \(f\) and \(g\).

Lemma: If there is a bijection between \(A\) and \(B\),
there is also a bijection between \(2^A\) and \(2^B\).
(Axiom of choice?)

http://math.stackexchange.com/questions/243590/bijection-from-mathbb-r-to-mathbb-rn

Lemma: If there is a bijection between \(A\) and \(B\),
there is also a bijection between \(A \to A\) and \(B \to B\).
(Since \((A \to A) \subset 2^A\)).

Conclusion: there is an isomorphism between the set of \((A,f)\)s and the set of \((A^2,g)\)s.

A homomorphism from \((A, f)\) to \((B, g)\) is \(h : A \to B\) such that
\(h~(f~x) = g~(h~x)\).

Let there be these structures:

* The unary system \((A, f)\) where \(f : A \to A\).
* The fixpointed unar \((A, f, p)\) where \(f~p = p\).
* The magma \((A, g)\) where \(g : A \to A \to A\).
* The semigroup \((A, g)\) where \(g\) is associative.
* The semigroup \((A, g, a)\) with left-absorbing element \(a\).
* The unar \((A^2, h)\) where \(A^2 = A \times A\).

A fixpoint in the unar becomes a left-absorbing element in the magma.

The semigroup is non-commutative: \(F~f~x~y \neq F~f~y~x\).

Therefore there is a homomorphism from
the algebra of unary systems to the algebra of non-commutative semigroups.

A left-absorbing element in the binar becomes
the left component of a fixpoint in the unar.
\[
(g~p~y, ~y) = (p,y) = h~(p,y)
\]

Another way to embed:
\[
\begin{align*}
(g~x~y, ~ g~y~x) = h~(x,y)
\\ (g~p~y, ~ g~y~p) = (p, ~ f~y) = h~(p,y)
\\ (g~x~p, ~ g~p~x) = (f~x, ~ p) = h~(x,p)
\end{align*}
\]

Flip, like negation:
\[
m~(x,y) = m~(y,x)
\]

Lemma: If \(c\) is a cardinal number, then \(c! = 2^c\).
[WolframAlpha for the factorial of aleph-0](https://www.wolframalpha.com/input/?i=aleph+0+factorial)

## A unar and its square are isomorphic?

\[
h~(g~x~y) = f~(h~x)?
\]

# Graph

There is always an injection from a unary algebra \((A, f)\)
to the *directed graph* \((A,E)\) where \(f~x = y\) iff \(E~x~y\).

There is always an injection from a directed graph \((V, E)\)
to the unary algebra \((2^V, F)\) where \(F~X = \{ y ~|~ x \in X, ~ E~x~y \}\).

# Every magma can be commutative

Every magma \((A,f)\) can be embedded into an \(n\)-anticommutative magma \((A^2,g,n)\)
where \(g~a~b = n~(g~b~a)\).

\[
\begin{align*}
n~(x,y) = (y,x)
\\ g~(u,v)~(x,y) = (f~u~v, f~x~y)
\end{align*}
\]

# Conclusion

There is an isomorphism between unary systems and magmas.
