---
title: Are all approximations truncation?
permalink: /atrunc.html
date: 2018-08-17 22:52 +0700
mathjax: yes
---

We can approximate a series by *truncating* it.

Suppose that the series \\( y = x_0 + x_1 + \ldots \\) converges.

Suppose that the sequence \\( \langle x_0, x_1, \ldots \rangle \\) converges to zero.

Pick where to cut.
Pick a natural number \\( n \\).

Then the series \\( x_0 + \ldots + x_n \\) approximates the series \\( y \\).
We cut its tail.
We take finitely many summands from the beginning.

Here come examples: Truncate all the series!

## Power series truncation

Below we truncate a power series.

Decimal truncation: \\( 1.2 \\) approximates \\( 1.23 \\).
Remember that a decimal number is a series.
For example, the number \\( 1.23 \\) is the power series
\\[ \ldots 01.230 \ldots = \ldots + 0 \cdot 10^1 + 1 \cdot 10^0 + 2 \cdot 10^{-1} + 3 \cdot 10^{-2} + 0 \cdot 10^{-3} + \ldots. \\]

Polynomial truncation: \\( 1 + x \\) approximates \\( 1 + x + x^2 \\) for \\( x \\) near zero.

Taylor series truncation: \\( 1 + x + \frac{x^2}{2} \\) approximates \\( e^x \\) for \\( x \\) near zero.
Remember the Taylor series expansion \\( e^x = \sum_{n \in \Nat} \frac{x^n}{n!} \\).

Below we truncate the ratio of two power series.

Rational truncation: \\( 12/23 \\) approximates \\( 123/234 \\).

[WP:Padé approximation](https://en.wikipedia.org/wiki/Pad%C3%A9_approximant) is a truncation of a ratio of series.

Fourier series truncation: The [Wikipedia example](https://en.wikipedia.org/wiki/Fourier_series#Example_1:_a_simple_Fourier_series) animates how a Fourier series converges to the sawtooth function as more terms are added.

Digression: Is a (complex) Fourier series a power series?
Reminder: A Fourier series looks like \\( \sum_{k=0}^{\infty} c_k e^{ikt} \\).

[WP:Laurent series](https://en.wikipedia.org/wiki/Laurent_series) truncation?

### Digression: What is an analytic function?

A function is *analytic* iff it can be represented by power series.

Formally, a function \\( f \\) is *analytic* iff for every \\( x \in \dom(f) \\), we can write \\( f(x) \\) as a power series.

See also [WP:Definition of "analytic function"](https://en.wikipedia.org/wiki/Power_series#Analytic_functions).

Taylor series expansion is illustrated in the 2015 slides "Taylor Series: Expansions, Approximations and Error" ([pdf](https://relate.cs.illinois.edu/course/cs357-f15/file-version/2978ddd5db9824a374db221c47a33f437f2df1da/media/cs357-slides6.pdf))

### Digression: What is the relationship between polynomial and power series?

A polynomial is an algebraic expression. It is not a function.

Power series is a kind of infinite polynomial.

[WP:Formal power series](https://en.wikipedia.org/wiki/Formal_power_series): "A formal power series is a generalization of a polynomial, where the number of terms is allowed to be infinite."

## Iteration truncation

- [WP:Iterated function](https://en.wikipedia.org/wiki/Iterated_function)
- [WP:Iterative method](https://en.wikipedia.org/wiki/Iterative_method)
- [Newton's Iteration](http://mathworld.wolfram.com/NewtonsIteration.html)
- [WP:Methods of computing square roots, the Babylonian method](https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method)
- An iteration converges to an attractive fixed point.

Example:
Let \\( f(x) = x + \frac{1}{x} \\).

Continued fraction truncation:
We know that \\[ 1 + \frac{1}{1 + \frac{1}{1 + \ldots}} = \frac{1 + \sqrt{5}}{2} = \Phi. \\]
We can truncate that continued fraction to approximate \\( \Phi \\).

Seeing those examples makes me wonder whether all approximations are truncation.
