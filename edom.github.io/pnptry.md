---
title: Trying to prove P neq. NP
permalink: /pnptry.html
date: 2018-04-28 22:30 +0700
mathjax: yes
---

**Note:**
This is a proof *attempt*, not a proof.

Last update was 2018-04-28.

## An attempt

Let <span>\(
\newcommand\SetOutcome{\mathbb{F}}
\newcommand\SetBit{\mathbb{B}}
\newcommand\SetPred{\mathbb{P}}
\newcommand\FunSat{\text{sat}}
\newcommand\FunMinTime{\text{MinTime}}
\newcommand\FunLen{\text{Len}}
\SetBit = \{ 0, 1 \} \)</span>
be the set of *bits*.

Let \\( \SetBit^* \\) be the *Kleene closure* of \\( \SetBit \\).

Let <span>\( \SetOutcome = \{ \text{accept}, \text{reject} \} \)</span> be the set of *final states*.

A *predicate* is a function in \\( \SetBit^* \to \SetBit \\).

Let \\( \SetPred \\) be the set of all *computable predicates*.

Let \\( p \in \SetPred \\) be a computable predicate.

Let \\( \FunLen(x) \\) be the *length* of the string \\( x \in \SetBit^* \\).

Let the function \\( \FunSat : \SetPred \times \Nat \to \SetOutcome \\) be

<div>\[
\FunSat(p,n) = \begin{cases}
    \text{accept} & \text{if \( \exists x \in \SetBit^n : p(x) = 1 \);}
    \\
    \text{reject} & \text{otherwise.}
\end{cases}
\]</div>

Let \\( \FunMinTime_M(p,x) \\) be the *shortest time* (the minimum number of steps)
required by machine \\( M \\)
to compute \\( p(x) \\) (to compute the predicate \\( p \\) with input \\( x \\)).

Let \\( N \\) be an NTM (non-deterministic Turing machine).

Let \\( D \\) be a DTM (deterministic Turing machine).

Such NTM \\( N \\) can compute \\( \FunSat(p,n) \\) in \\( O(n + \max_{x \in \SetBit^n} \FunMinTime_N(p,x)) \\) steps.
This is such algorithm:

```
function sat (p, n) {
    var x: array [1..n] of bit
    for i := 1 to n {
        x[i] := guess
    }
    if p(x) { accept }
    else { reject }
}
```

Such DTM \\( D \\) can compute \\( \FunSat(p,n) \\) in \\( O(\sum_{x \in \SetBit^n} \FunMinTime_D(p,x)) \\) steps.
This is such algorithm:

```
function sat (p, n) {
    for x in B^n {
        if p(x) { accept }
    }
    reject
}
```

**Conjecture:** There exists a computable predicate \\( p \in \SetPred \\) such that
1. \\( \FunMinTime_D(p,x) = \FunMinTime_N(p,x), \\)
1. \\( \FunMinTime_D(p,x) \in O([\FunLen(x)]^k) \\) where \\( k > 1 \\),
1. \\( N \\) optimally computes \\( \FunSat(p,n) \\) in \\( O(n^k) \\) time, and
1. \\( D \\) optimally computes \\( \FunSat(p,n) \\) in \\( O(2^n \cdot n^k) \\) time.

If that conjecture is true, then \\( \TimeP \neq \TimeNP \\).

## Questions

Is there a problem whose optimal solution is exhaustive search?

Can we apply pigeonhole principle to the computation graph?

What problems are equivalent to the P vs NP problem?

## Plan

- Reading list
    - [2010 Ryan Williams "Improving Exhaustive Search Implies
    Superpolynomial Lower Bounds"](https://www.cs.cmu.edu/~ryanw/improved-algs-lbs2.pdf):
    "The P vs NP problem arose from the question of whether exhaustive search is necessary for problems
    with short verifiable solutions."
    - [WP: Natural proof](https://en.wikipedia.org/wiki/Natural_proof)
    - approaches:
        - Fagin, Immerman, ...: descriptive complexity theory
        - Mulmuley, Sohoni, ...: geometric complexity theory
            - [2007 Mulmuley and Sohoni, "Geometric Complexity Theory: Introduction"](https://arxiv.org/abs/0709.0746)
            - [2013 Landsberg "Geometric Complexity Theory: an introduction for geometers"](https://arxiv.org/abs/1305.7387)
            - [2015 Landsberg "An introduction to geometric complexity theory"](https://arxiv.org/abs/1509.02503)
            - [Kenneth W. Regan's "Understanding the Mulmuley-Sohoni Approach to P vs. NP"](https://www.cse.buffalo.edu//~regan/papers/pdf/Reg02MSFD.pdf)
            - [Christian Ikenmeyer's 2018 course](https://people.mpi-inf.mpg.de/~cikenmey/teaching/summer18/firstintrotogct/index.html)
    - [A clearing house for Deolalikar P vs NP paper](http://michaelnielsen.org/polymath1/index.php?title=Deolalikar_P_vs_NP_paper)
- Undigested
    - One-way function implies something about P vs NP?

## Meta-research

- Where are progress tracked?
    - [2017 Richard J. Lipton and Kenneth W. Regan](https://rjlipton.wordpress.com/2017/02/05/a-panel-on-p-vs-np/)
    - [2016 Gerhard J. Woeginger](http://www.win.tue.nl/~gwoegi/P-versus-NP.htm)
    - [2015 Lance Fortnow and Bill Gasarch](http://blog.computationalcomplexity.org/2015/08/have-we-made-progress-on-p-vs-np.html)
    - [2014 reddit](https://www.reddit.com/r/math/comments/1krrkx/what_progress_has_been_made_on_the_p_vs_np/)
    - [2009 Lance Fortnow "The status of the P versus NP problem"](http://www.ncmis.cas.cn/kxcb/jclyzs/201204/W020120424627425387644.pdf)
- What is the P vs NP problem?
    - Official problem description: [The P versus NP problem, by Stephen Cook, for the Clay Millennium Prize Problems](http://www.claymath.org/sites/default/files/pvsnp.pdf)

## Another attempt?

- This is an older attempt.
- This should be merged to the attempt above.
- Let:
    - \\( f \\) be a predicate
    - \\( k \\) be a natural number
    - \\( Sat(f,k) \\) be the problem of finding a string \\( x \\) of length \\( k \\) such that \\( f(x) = 1 \\)
- Lemma: If \\( f \in \TimeP \\) then \\( Sat(f,k) \in \TimeNP \\).
(This should be obvious and simple to prove?)
- Conjecture: There exists a predicate whose search cannot be faster than brute force.
    - Formally: There exists \\( f \in \TimeP \\) such that \\( Sat(f,k) \not \in \TimeP \\).
- That lemma and that conjecture, if proven true, would imply \\( \TimeP \subset \TimeNP \\).
- We try to prove that conjecture by diagonalization/pigeonholing?
The set <span>\( \{0,1\}^k \to \{0,1\} \)</span> has \\( 2^{2^k} \\) elements,
because by combinatorics, in the truth table, there are \\( 2^k \\) rows, and each row has \\(2\\) possibilities.
There are \\( 2^{2^k} \\) possible \\(k\\)-letter-string predicates.
Suppose that a deterministic machine can solve \\( Sat(f,k) \\) for all \\(f\\) in \\(O(poly(k))\\) time.
(Can we apply pigeonhole principle to the configuration graph?)
- Every predicate can be stated in disjunctive normal form.

## Other people's works that may be related

- 2017-11-22 news about NEXP and ACC https://news.mit.edu/2017/faculty-profile-ryan-williams-1122
- an explanation in English https://danielmiessler.com/study/pvsnp/
