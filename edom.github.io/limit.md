---
title: Limit
permalink: /limit.html
date: 2017-06-27 03:00:00 +0700
mathjax: yes
---

# Limit

We motivate the definition of the limit of a sequence.

A *sequence* \\( s \\) consists of the elements \\( s_0 \\), \\( s_1 \\), and so on, possibly endlessly.
The notation <span>\( s = ( 1/n )_{n=1}^{\infty} \)</span> means the sequence
<span>\begin{align*}
s_1 &= 1/1,
\\ s_2 &= 1/2,
\\ s_3 &= 1/3,
\end{align*}</span>
and so on where \\( s_n = 1/n \\) for each integer \\( n \\) greater than or equal to one.
The index does not have to begin with one.
For example, the notation <span>\( ( n^2 )_{n \in \Nat} \)</span> means the sequence
<span>\begin{align*}
s_0 = 0^2,
\\ s_1 = 1^2,
\\ s_2 = 2^2,
\end{align*}</span>
and so on where \\( s_n = n^2 \\) for each \\( n \\) in \\( \Nat \\).
Unless indicated otherwise, an index is a natural number, which begins from zero.
Instead of using indexes such as \\( s_n \\), we can also use brackets \\( s[n] \\),
and we can even use parentheses \\( s(n) \\).
We define \\( S(A) \\) to be the image of \\( A \\) through \\( s \\),
that is the set of all possible outputs of \\( s \\),
that is the set <span>\( \{ x ~|~ a \in A, ~ s(a) = x \} \)</span>.
The set \\( S(A) \\) is obtained by applying \\( s \\) to each element of \\( A \\),
that is, if <span>\( A = \{ a_1, a_2, a_3, \ldots \} \)</span>, then <span>\( S(A) = \{ s(a_1), s(a_2), s(a_3), \ldots \} \)</span>.
Let \\( A \\) be a set from which the sequence elements are taken.
Let \\( A \\) also has a partial order \\( \le \\).
The statement \\( \beta \le \alpha \\) can also be written \\( \alpha \ge \beta \\).
The notation \\( s(k) \\) suggests that a sequence can be thought as a function \\( \Nat \to A \\).
Due to the partial order, we can define an *upper bound of \\( s \\)* as a \\( z \\) such that \\( s_n \le z \\) for all \\( n \\),
and we can define a *lower bound of \\( s \\)* as an \\( a \\) such that \\( s_n \ge a \\) for all \\( n \\).
The *supremum* of \\( s \\) is its least upper bound.
The *infimum* of \\( s \\) is its greatest lower bound.
If and only if the infimum \\( \inf s \\) and the supremum \\( \sup s \\) have the same value \\( L \\),
then we say that \\( s \\) *converges* to \\( L \\),
and we say that the *limit* of \\( s \\) is \\( L \\),
and we write \\( \lim s = L \\).

Note that the bounds do not have to occur in the sequence itself.

If we have a sequence \\( s \\),
then we can apply a function \\( f \\) to each member of \\( s \\)
to create a new sequence \\( t_n = f(s_n) \\).
Every function can be *lifted* into a function that works on sequences
by applying that original function to each element of the sequence.

Now that we have defined the limit of a sequence,
we can now define the limit of a function *as its input approaches a value*.
Let \\( f \\) be a function.
By saying "the input of \\( f \\) approaches \\( a \\)",
we mean that there exists a sequence \\( x \\) that converges to \\( a \\).
We define the *limit* of a function \\( f \\) as its input \\( x \\) approaches \\( a \\) to be \\( L \\), written \\( \lim_{x \to a} f(x) = L \\),
if and only if for every sequence \\( u \\) that converges to \\( a \\),
the limit of the sequence <span>\( \lim (f(u_n))_{n \in \Nat} = f(a) \)</span>.

Another way we can define \\( \lim_{x \to a} f(x) \\) is by the epsilon-delta definition of limit
as done by Cauchy, Bolzano, and Weierstrass.

A *generalized sequence* is a function from an index set \\( I \\) to an element set \\(A \\),
where the index has an ordering \\( \le \\).

## Related materials

- Wikipedia
    - [Sequence: Examples and notation](https://en.wikipedia.org/wiki/Sequence#Examples_and_notation)
    - [Limit](https://en.wikipedia.org/wiki/Limit_(mathematics))
    - [Topological net](https://en.wikipedia.org/wiki/Net_(mathematics))
