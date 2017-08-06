---
title: Division from plus, times, and norm
permalink: /division.html
date: 2017-03-20 03:24:26 +0700
mathjax: yes
---

## Abstract

If we have multiplication, addition, and preorder, then we can define division.
(The multiplication does not have to commute.
A preorder is a binary relation that is reflexive and transitive.)
To divide \\(n\\) by \\(d\\) is to find
\\((q,r)\\) that satisfies \\(n = q \times d + r\\)
and minimizes the norm of \\(r\\).
This allows us to generalize division to integers and sets.

## Terminology

In \\(a \times b = c\\),
we call \\(a\\) the multiplier, \\(b\\) the multiplicand, and \\(c\\) the product.
The multiplicand is the thing that is repeated.
The multiplier is how many times the multiplicand is repeated.
Therefore \\(2 \times 3 = 3+3\\) and \\(3 \times 2 = 2+2+2\\).

When we write \\(p \times q\\), we say that we *left-multiply \\(q\\) by \\(p\\)*,
and we can also say that we *right-multiply \\(p\\) by \\(q\\)*.
The "left" and "right" tell us where the multiplier is.
In a left multiplication, the multiplier (the thing that multiplies)
is on the left side of the multiplicand (the thing that is multiplied).
We say that we left-multiply or right-multiply a multiplicand *by* a multiplier.

In \\(n / d = q\\),
we call \\(n\\) the dividend,
\\(d\\) the divisor,
and \\(q\\) the quotient.
The dividend is the thing that is divided.
The divisor is the thing that divides.
The word "quotient" comes from the Latin word "quotiens" that means "how many times",
which suggests that the quotient describes *how many times* we must repeat the divisor to obtain the dividend.
Therefore \\(6/2 = 3\\) because \\(2+2+2 = 6\\)
and \\(6/3 = 2\\) because \\(3+3 = 6\\).
For example, 6 balls divided by 2 boxes is 3 balls *per* box.

The divisor in a division
becomes the multiplicand in the related multiplication.
Formally, \\(n / d = q\\) iff \\(n = q \times d\\).
To obtain the related multiplication of a division,
*right*-multiply each side of the equation by the divisor.
Right multiplication cancels right division.

In the case of left division and right division,
the "left" and "right" tell us where the divisor is.
Left division means the divisor is on the left side of the dividend.

If \\(p \times (q \times r) = r\\), we call \\(p\\)
the *left inverse* of \\(q\\) because \\(p\\)
cancels a multiplication where \\(q\\) is on the *left* side.
We also say that we *left-divide* \\(q \times r\\) by \\(q\\).
We also write \\(q \backslash (q \times r) = r\\).

Similarly, if \\((p \times q) \times r = p\\), we call \\(r\\)
the *right inverse* of \\(q\\) because it
cancels a multiplication where \\(q\\) is on the *right* side.
We write \\((p \times q) / q = p\\).

To *left-divide* a dividend by a divisor is
to *left-multiply* the dividend by the *left-inverse* of the divisor.
To *right-divide* a dividend by a divisor is
to *right-multiply* the dividend by the *right-inverse* of the divisor.

The consequence of those definitions is that
\\(6 / 2 = 3\\) is
because \\(3 \times 2 = 6\\),
not because \\(2 \times 3 = 6\\).

A problem with this division is that
it is not always defined for every non-zero divisor.
Here the *remainder* comes into play
and allows us to extend the definition of division.
The remainder is the smallest thing that we must add to satisfy the equation
after we find the quotient.
If the remainder is zero, we say that the divisor *divides* the dividend.

The expression \\(2 \times 3\\) means \\(3+3\\), not \\(2+2+2\\).
They both evaluates to \\(6\\), but they mean different things.
For natural numbers this sounds pedantic,
but this is important when the multiplication does not commute,
such as set multiplication that we shall see later.

We define \\(n/d = q\\) where \\(n = q \times d + r\\)
such that \\(r\\) is *minimal* in a way.

## Structure

Let there be a ring-like structure.
Let there be a set \\(R\\),
an addition \\(+\\),
a multiplication \\(\times\\),
and a norm \\(m : R \to [ 0,\infty )\\).
The operations do not have to have identities.
They do not have to be invertible.
They do not have to commute.

Dividing \\(n\\) by \\(d\\) is finding \\((q,r)\\) such that \\(n = q \times d + r\\)
and making sure that such \\((q,r)\\) is unique.
However, there may be many \\((q,r)\\) that satisfies that equation.
We want at most one pair.
Which pair should we choose as the result of the division?

If \\(R = \Nat\\), we choose the smallest \\(r\\).
If \\(R = \Int\\), we choose the \\(r\\) that minimizes \\(|r|\\),
the absolute value of \\(r\\).
If \\(R\\) is a set, we choose the \\(r\\)
that minimizes \\(|r|\\), the size of \\(r\\).
The common theme here is that we can define a *norm*.
The division then picks the pair that minimizes the norm of the remainder.
Here we write \\(|r|\\) (the norm of \\(r\\))
to mean the distance between zero (the additive identity) and \\(r\\).
The norm of a natural number is the number itself.

That equation is a special case of the linear equation
\\( b = a_1 \cdot x_1 + \ldots + a_p \cdot x_p \\)
where \\(b = n\\), \\(p = 2\\), \\(a = (d,1)\\), \\(x = (q,r)\\).

## Generalization

To understand division,
first we look at following example about the natural numbers.
Later we will generalize the division to also work on sets.

Suppose that we want to divide \\(n\\) by \\(d\\).
Whenever we divide \\(n\\) by \\(d\\),
what we actually do is we
find \\(q\\) and \\(m\\) that satisfy
\\(n = q \times d + m\\)
where \\(q\\) is \emph{maximal}
and \\(m\\) is \emph{minimal}
in the sense that there are no bigger \\(q\\) and smaller \\(m\\)
that satisfy the equation.
We call \\(n\\) the \emph{dividend} (the thing that is divided),
\\(d\\) the \emph{divisor} (the thing that divides),
\\(q\\) the \emph{quotient},
and \\(m\\) the \emph{modulus} or the \emph{remainder}.

Now consider the integers.
Given \\(n \neq 0\\) and \\(d \neq 0\\),
there are infinitely many \\((q,m)\\) pairs that satisfy \\( n = q \times d + m \\).
Which one should we choose?

We have just defined natural number division in terms of multiplication, addition, and natural ordering.
If we can define set multiplication, set addition, and set ordering, then we can define set division.

We can define the multiplication of two sets as their Cartesian product.
We can define the addition of two sets as their union.
We can define that a set is less than another iff the former is a proper subset of the latter.
We then define the remainder as the smallest set that we must add to satisfy the equation.
Formally,
<span>\begin{align*}
    Q \times D &= \{ (q,d) ~|~ q : Q, ~ d : D \}
    \\
    A + B &= \{ x ~|~ x : A \vee x : B \}
    \\
    A \le B &\iff A \subseteq B
    \\
    N &= Q \times D + R \text{ such that \(R\) is minimal according to \(\le\).}
\end{align*}</span>

To compute the quotient and the modulus,
we solve \\(N = Q \times D + R\\)
with the constraint that \\(R\\) is \emph{minimal}
in the sense that there are no smaller \\(R\\)
that satisfies that equation.

Similar to \\(n/d = q\\), we define \\( N / D = Q \\).
It seems that all we did was rewrite everything using capital letters,
but this simplicity is deceiving:
the variables now represent sets instead of numbers.

## Examples

### Set division

Let's say we have these:
<span>\begin{align*}
    N &= \{ a, b, c, aa, ab, ac, ba, bb, bc, ca, cb, cc \}
    \\
    D &= \{ a, b \}
\end{align*}</span>
We compute:
<span>\begin{align*}
    N &= Q \times D + M
    \\
    N &= \{ a, b, c \} \times \{ a, b \} + \{ c, ac, bc, cc \}
\end{align*}</span>
We can't shrink \(M\) anymore
so we claim:
<span>\begin{align*}
    Q &= \{ a, b, c \}
    \\
    M &= \{ a, b, c, ac, bc, cc \}
\end{align*}</span>

### Polynomial division

Let's say we have a polynomial \\(x^2 + 2x + 3\\) and we want to divide it with \\(x + 1\\).
<span>\begin{align*}
    n &= x^2 + 2x + 3
    \\
    d &= x + 1
    \\
    q &= x + 1
    \\
    r &= 1
\end{align*}</span>
We define \\(p \le q\\) iff \\(\deg~p \le \deg~q\\) or \\(p_1 \le q_1 \wedge \ldots\\).

### Language division

The left quotient
of a language \\(L\\) with respect to a string \\(p\\) is \\(\{p\} \backslash L\\).
The language \\( p \backslash L \\) is the set of every string \\( x \\)
that would be an element of \\( L \\) if \\( p \\) were prepended to \\( x \\).
Such left division is the special case of left-division of languages
where the divisor has only one element.
<span>\begin{align*}
    \{p\} \backslash L &= \{ x ~|~ px \in L \}
    \\
    P \backslash L &= \{ x ~|~ px \in L, ~ p \in P \} ?
\end{align*}</span>

The Brzozowski quotient is a special case where the divisor is a singleton set:
<span>\( \{p\} \backslash L \)</span>.

## Related materials

- Wikipedia
    - [Division](https://en.wikipedia.org/wiki/Division_(mathematics))
    - [Quotient of a formal language](https://en.wikipedia.org/wiki/Quotient_of_a_formal_language)
    - [Brzozowski derivative](https://en.wikipedia.org/wiki/Brzozowski_derivative)
        - [Janusz Brzozowski et al. on arxiv](https://arxiv.org/find/cs/1/au:+Brzozowski_J/0/1/0/all/0/1)
    - [Preorder](https://en.wikipedia.org/wiki/Preorder)
