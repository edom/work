---
title: Stop abusing the equals sign
date: 2018-04-12 00:00 +0700
permalink: /equals_sign.html
mathjax: yes
---

The equals sign should be used for equations only and nothing else.

Consider this fragment in a hypothetical functional programming language.

```haskell
hang : Nat
hang = hang
```

The fragment `hang = hang` falsely suggests that it is an *equation* like \\( x = x \\).
We can substitute \\( x \\) with any natural number, and the equation \\( x = x \\) will still hold.
But that is not what we mean with `hang`.
We are defining a *rewrite rule*, not an equation.
Thus we should write `hang => hang` instead.

In an equation, the left and right side are equal.
We don't care if they are flipped.
In a rewrite rule, we care about direction.
We want to start with \\( 1 + 1 \\) and end with \\( 2 \\), not the other way,
unless we are writing a theorem prover.

Programming is hard enough already.
Let's not make it harder for students.
Stop using confusing notation.

Reading triage:

- [Misuse of the equals sign: An entrenched practice from early primary years to tertiary mathematics](https://www.researchgate.net/publication/286418817_Misuse_of_the_equals_sign_An_entrenched_practice_from_early_primary_years_to_tertiary_mathematics)
