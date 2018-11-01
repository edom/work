---
title: Circuit complexity
mathjax: yes
date: 2017-06-29 18:30 +0700
permalink: /circuit.html
---

# Circuit complexity

- [WP:Circuit complexity](https://en.wikipedia.org/wiki/Circuit_complexity)

## The shortest \\(n\\)-parameter boolean predicate equivalence class representative problem

- Let \\(E_n\\) be the set of all *Boolean \\(n\\)-expressions*.
    - An *\\(n\\)-expression* is an expression that has at most \\(n\\) variables.
    - Formally, the syntax of \\( E_n \\) is:
        - Constant expressions:
            - \\( 0 \in E_n \\).
            - \\( 1 \in E_n \\).
        - Variable expressions:
            - If \\( k \in \Nat \\) and \\(0 \le k < n\\), then \\(x_k \in E_n\\).
                - The expression \\(x_k\\) is purely symbolic.
                    - The \\(x\\) does not mean anything.
        - If \\(\alpha \in E_n\\), then \\((\neg \alpha) \in E_n\\).
        - If \\(\alpha \in E_n\\) and \\(\beta \in E_n\\), then \\((\alpha \wedge \beta) \in E_n\\).
        - If \\(\alpha \in E_n\\) and \\(\beta \in E_n\\), then \\((\alpha \vee \beta) \in E_n\\).
        - Nothing else is in \\(E_n\\).
- Let the *size* of a formula be the number of operators in it.
    - We write \\(C(\phi)\\) for the size of the formula \\(\phi\\).
    - We say that \\(\alpha\\) is *smaller* than \\(\beta\\) iff \\(C(\alpha) < C(\beta)\\).
    - Formally we define \\(C(\phi)\\) as:<span>\begin{align*}
C(\neg \alpha) &= 1 + C(\alpha),
\\
C(\alpha \wedge \beta) &= 1 + C(\alpha) + C(\beta),
\\
C(\alpha \vee \beta) &= 1 + C(\alpha) + C(\beta),
\\
C(\alpha) &= 0 \text{ otherwise}.
\end{align*}</span>
- Given a formula \\( \phi \in E_n \\) and an *assignment* \\( a : \{0,1\}^n \\),
we can *interpret* the formula \\( \phi \\).
    - The result of interpreting \\( \phi \\) with assignment \\( a\\) is written \\( \phi|_a \\),
    and is obtained by replacing each \\( x_k \\) with \\( a_k \\)
    and evaluating the expression to either zero or one.
    - This interpretation enables us to define *equivalence*,
        - Formally, we say that two formulas \\( \alpha, \beta \in E_n \\) are *equivalent*, written \\( \alpha \equiv \beta \\), iff
        for every assignment \\( a \in \{0,1\}^n \\), it holds that \\( \alpha|_a = \beta|_a \\).
        Then, we define the *equivalence class* of a formula \\( \phi \in E_n \\) as
        \\(
        [\phi] = \{ \alpha ~|~ \alpha \equiv \phi, ~ \alpha \in E_n \}
        \\).
            - Every element of that equivalence class is called a *representative* of that class.
            Note that equivalence is not equality: \\( x_0 \wedge x_0 \\) and \\( x_0 \\)
            are equivalent but not equal.
        - Two formulas are equivalent iff they always give matching results for all assignments.
        - Two formulas are equal iff they look the same.
        - Equivalence is the comparison of meaning,
        whereas equality is the comparison of form.
- The set \\(E_n\\) has exactly \\(2^{2^n}\\) equivalence classes.
    - We label those classes \\( K(n,0), K(n,1), \ldots K(n,2^{2^n}-1) \\).
    - Define \\( Q(n,k) \\) as the shortest representative of \\( K(n,k) \\).
        - Here are some examples of the shortest representatives
        that can be verified by hand:<span>\begin{align*}
Q(0,0) &= 0
\\ Q(0,1) &= 1
\\ Q(1,0) &= 0
\\ Q(1,1) &= x_0
\\ Q(1,2) &= \neg x_0
\\ Q(1,3) &= 1
\\ Q(2,1) &= x_0 \wedge x_1
\\ Q(2,2) &= \neg x_0 \wedge x_1
\\ Q(2,3) &= x_0
\\ Q(2,6) &= \neg (x_0 \wedge x_1) \wedge (x_0 \vee x_1)
\\ Q(2,7) &= x_0 \vee x_1
\\ Q(2,9) &= (x_0 \wedge x_1) \vee \neg (x_0 \vee x_1)
\end{align*}</span>
- It should be apparent that \\( Q(n,2^{2^n}-1-k) = \neg Q(n,k) \\).
It should be apparent that \\( Q(2,6) \\) is XOR and \\( Q(2,9) \\) is bi-implication.
It should be apparent that \\( Q(2,6) \\) and \\( Q(2,9) \\) are the longest expressions for \\( n = 2 \\),
and both of them have size \\( 4 \\).
- **Problem statement**:
For each \\(n\\),
find \\(k\\)
such that \\(Q(n,k)\\) is the longest among all possible \\(k\\).
- [WP:Parity function](https://en.wikipedia.org/wiki/Parity_function)?
> The \\(n\\)-variable parity function and its negation are the only Boolean functions for which
all disjunctive normal forms have the maximal number of \\( 2^{n - 1} \\) monomials of length \\( n \\)
and all conjunctive normal forms have the maximal number of \\( 2^{n - 1} \\) clauses of length \\( n \\).
(Ingo Wegener, Randall J. Pruim, *Complexity Theory*, 2005, ISBN 3-540-21045-8, p. 260)
- Relationship between \\(n\\)-expressions and \\(n\\)-cubes
    - Here we imagine what it is like to apply geometric operations to Boolean expressions.
    - Draw
        - Draw the outline of a square on a white paper.
        - Draw two lines that divide the square into four smaller subsquares.
        - Color the top right subsquare red.
            - The resulting picture represents \\( x_0 \wedge x_1 \\).
    - The result of rotating \\( x_0 \wedge x_1 \\) 90 degrees counterclockwise is \\( \neg x_0 \wedge x_1 \\).
        - Rotated once again, it becomes \\( \neg x_0 \wedge \neg x_1 \\).
        - Rotated once again, it becomes \\( x_0 \wedge \neg x_1 \\).
        _ Rotated once again, it comes back to \\( x_0 \wedge x_1 \\).
    - Define \\( R_2(\phi) \\) as the counterclockwise-rotated \\( \phi \\) where \\( \phi \in E_2 \\).
        - Then \\( R(x_0) = x_1 \\) and \\( R(x_1) = \neg x_0 \\).
    - Other operations:
    horizontal flip,
    vertical flip,
    negation.
    - \\( (R_2)^4(\phi) = \phi \\).
    - On the 3-cube, there are 3 counterclockwise rotations.
    - Each \\(n\\)-expression of the form \\(x_k\\) divides the \\(n\\)-cube into two region.
- Unnecessary
    - We define the shorthand \\(\alpha < \beta\\) to mean that \\(\alpha\\) is shorter than \\(\beta\\).
    Now we can *order* the equivalence classes in \\(E_n\\) by their sizes.
    For every \\(E_n\\), there exists at least one *infimum* (greatest lower bound).
    For every \\(A \subseteq E_n\\),
    we say that \\(\alpha \in \inf(A)\\) iff \\( \alpha \le \phi \\) for every \\(\phi \in E_n\\).
    - A *bit* is either zero or one.
    - We define the mapping \\( N_n : \{0,1\}^n \to \Nat \\)
    as a mapping from the \\(n\\)-dimensional bit vector \\(x = (x_0,\ldots,x_{n-1})\\)
    to the natural number \\(N_n(x) = \sum_{k=0}^{n-1} x_k 2^k \\).
        - \\(N_n(x)\\) is the number whose
        \\(n\\)-bit binary right-to-left encoding is the \\(n\\)-dimensional bit vector \\(x\\).
    - We define the bit vector identifying the predicate as
    <span>\begin{align*}
    B_n(\phi) = N_{2^n}( \phi(N_n^{-1}(0)), \ldots, \phi(N_n^{-1}(2^n-1)))
    \end{align*}</span>
- Shannon 1949 proved that almost all \\( n \\)-argument boolean functions
require circuits of size \\( \Theta(2^n/n) \\). (citation?)
- For small numbers, we can enumerate the answers by hand.
- A formula is *canonical* iff it cannot be shortened.

What is the longest possible canonical description length of a predicate that takes \\( k \\) arguments?

```
0000 | 0
0001 | a \wedge b
0010 | a \wedge \neg b
0011 | a
0100 | \neg a \wedge b
0101 | b
0110 | (a \wedge \neg b) \vee (\neg a \wedge b)
0111 | a \vee b
1000 | \neg (a \vee b)
1001 | (a \wedge b) \vee \neg (a \vee b)
1010 | \neg b
1011 | a \vee \neg b
1100 | \neg a
1101 | \neg a \vee b
1110 | \neg (a \wedge b)
1111 | 1
```

Conjecture: The longest 2-argument predicate is 0110.

Conjecture: \\( (a \wedge \neg b) \vee (\neg a \wedge b) \\) is the shortest description of 0110.

What we are asking here is Sipser 1997's *circuit-size complexity*?

- How are circuit complexity and proof complexity related?
- Simplification rewrite rules:
<span>\begin{align*}
\neg (\neg \alpha) = \alpha
\\
\alpha \wedge \neg \alpha = 0
\\
\alpha \vee \neg \alpha = 1
\\
\alpha \vee 1 = 1
\\
\alpha \wedge 0 = 0
\\
\neg 0 = 1
\\
\neg 1 = 0
\\
\neg \alpha \wedge \neg \beta = \neg (\alpha \vee \beta)
\\
\neg \alpha \vee \neg \beta = \neg (\alpha \wedge \beta)
\\
(\alpha \wedge \beta) \vee (\alpha \wedge \gamma) = \alpha \wedge (\beta \vee \gamma)
\end{align*}</span>
- Is this the problem we're talking about?
    - [WP:Circuit minimization for Boolean functions](https://en.wikipedia.org/wiki/Circuit_minimization_for_Boolean_functions)
    - [Circuit Minimization Problem](http://www.cs.sfu.ca/~kabanets/papers/mincircuit.pdf), 1999, Valentine Kabanets and Jin-Yi Cai
    - [Yale CS Circuit Minimization Team Work](http://www.cs.yale.edu/homes/peralta/CircuitStuff/CMT.html)
    - what?
        - [WP:Infimum and supremum](https://en.wikipedia.org/wiki/Infimum_and_supremum#Formal_definition)
