---
title: Function
date: 2018-04-07 00:00 +0700
permalink: /function.html
mathjax: yes
---

## Prerequisites

- [Wikipedia: Cartesian product](https://en.wikipedia.org/wiki/Cartesian_product)

## Relation

An example ordered pair is \\( (a,b) \\). It is not the same as \\( (b,a) \\).

A *relation* \\( r \\) is a triple \\( (A,B,R) \\) where
\\( A \\) is a set called the *domain*,
\\( B \\) is a set called the *codomain*,
and \\( R \subseteq A \times B \\) is a set that is the relation's *mapping*.
Such mapping is a set of ordered pairs.

Iff \\( (a,b) \in R \\), then we say that \\( r \\) *relates* \\( a \\) *to* \\( b \\).
The word "to" implies that direction is important.

A relation is not just a subset of a Cartesian product.
The domains and codomains matter.

## Function

A *function* is a relation in which each element of the domain is related to *exactly one* thing.

Let \\( f = (A,B,F) \\) be a function.

The notation of "applying \\( f \\) to \\( x \\)", written \\( f(x) \\),
means the \\( y \\) such that \\( (x,y) \in F \\).

## Partial functions

A *partial function* is a relation in which each element of the domain is related to *at most one* thing.
If you replace "at most one" with "exactly one", you get the definition of a function.

If a partial function \\( f \\) does not relate a domain element \\( x \\) to anything,
then we say that \\( f(x) \\) is *undefined*.

The symbol \\( \bot \\) is called *bottom*.

We can *totalize* a partial function \\( f = (A,B,F) \\) into a total function \\( f_\bot = (A,B_\bot,F_\bot) \\)
where \\( B_\bot = B \cup \\{ \bot \\} \\), and we require that \\( \bot \not\in B \\),
and, for each \\( a \in A \\):
1. if \\( (a,b) \in F \\), then \\( (a,b) \in F_\bot \\);
2. otherwise, \\( (a,\bot) \in F_\bot \\).

What is the difference between "undefined" and "bottom"?
The "bottom" of a set \\(B\\) is *defined* as something that is not equal to anything in \\( B \\).
On the other hand, something "undefined" is *not defined* at all,
for example the result of \\( 1 / 0 \\).
Thus, we should *not* write \\( f(x) = \bot \\) to mean that \\( f(x) \\) is undefined.

[Agda Wiki: Totality](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.Totality)

[ncatlab: partial function](https://ncatlab.org/nlab/show/partial+function)

The function \\( f : A \to B \\) has input type \\( A \\) and output type \\( B \\).

## Function equality

- Two relations are *equal* iff
    - their domains are equal,
    - their codomains are equal, and
    - their mappings are equal.
- Formally, \\( (A_1,B_1,R_1) = (A_2,B_2,R_2) \\) iff
    - \\( A_1 = A_2 \\),
    - \\( B_1 = B_2 \\), and
    - \\( R_1 = R_2 \\).
- Functions are relations.
    - Equality of functions is equality of relations.
- Intension vs extension
    - Consider:
        - \\( f : \Nat \to \Nat \\), <span>\( f(n) = n + n \)</span>,
        - \\( g : \Nat \to \Nat \\), <span>\( g(n) = 2 \cdot n \)</span>.
    - Observe:
        - They are extensionally equal: \\( f(n) = g(n) \\) for all \\( n \\).
        - They are not intensionally equal: \\( f \neq g \\).
        - Their outputs match, but they are not the same function.
    - Problem:
        - When do we care about intension?
        - Do we ever care at all?

## Relationship between functions in functional programming and functions in mathematics

These functions `idn` and `idz` are *different* functions.

```haskell
idn : Nat -> Nat
idn x => x

idz : Int -> Int
idz x => x
```

<span>\begin{align*}
[idn] = (\Nat, \Nat , x \to x)
\\
[idz] = (\Integers, \Integers, x \to x)
\end{align*}</span>

The slides [the lambda calculus](http://www.cs.yale.edu/homes/hudak/CS430F07/LectureSlides/Reynolds-ch10.pdf)
has some explanation of the semanticses of lambda calculus.
The slides are a part of [Yale CS-430/CS-530 formal semantics course](http://www.cs.yale.edu/homes/hudak/CS430F07/).
