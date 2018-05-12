---
title: Coq
permalink: /coq.html
date: 2017-07-17 23:00:00 +0700
mathjax: true
---

- Introduction
    - I haven't found a suitable introduction to Coq.
    - [Daniel Schepler's "Mathematical formalization using Coq"](https://people.debian.org/~schepler/coqtut.v.html) seems approachable.
    - [MO 155909: Wanted: a “Coq for the working mathematician”](https://mathoverflow.net/questions/155909/wanted-a-coq-for-the-working-mathematician)
    - [MO 164959: How do I verify the Coq proof of Feit-Thompson?](https://mathoverflow.net/questions/164959/how-do-i-verify-the-coq-proof-of-feit-thompson)
    - https://softwarefoundations.cis.upenn.edu/current/index.html
    - Yves Bertot's "Coq in a hurry"
    - [Coq tutorial by Mike Nahas](https://coq.inria.fr/tutorial-nahas)
    - [Calculus of inductive constructions](https://coq.inria.fr/distrib/current/refman/Reference-Manual006.html#Cic)
- [Coq and simple group theory](http://blog.mikael.johanssons.org/coq-and-simple-group-theory.html)
- Installation on Ubuntu 14.04
    - `sudo apt-get install coq`

## Introduction

Coq source file extension is `.v`.

The notation `x:T` means "the type of `x` is `T`", "`x` inhabits `T`".
It also means "`x` is a proof of `T`" by Curry-Howard isomorphism.

The type of `nat` is `Type(1)`.

The type of `set` is `Type(2)`.

The type of `Type(i)` is `Type(i+1)`.

```coq
(* This is a comment in Coq. *)
```

```
coqtop -l filename.v -batch
```

## Defining things

```coq
Definition x: nat := 0.
Check x.

Definition f (x: nat): nat := x + 1.
```

Definition = non-recursive definition

Fixpoint = recursive definition

Inductive = type (Set (small set)) definition

```coq
Definition name: type
where
type: Set
or type: Prop
or type: Type.
```

Proving 0+1 = 1 using Curry-Howard isomorphism?

Axiom: and-elimination

<div>\begin{align*}
a \wedge b \vdash a
\end{align*}</div>

```coq
Theorem and_elim: forall a b: Prop, a /\ b -> a.
tauto.
Qed.
```

<div>\begin{align*}
a \vdash a \vee b
\end{align*}</div>

Modus ponens

<div>\begin{align*}
a, a \rightarrow b \vdash b
\end{align*}</div>

## How Coq represents propositions

```coq
Print False.
Print True.
Inductive True : Prop := I : True.
Inductive False : Prop := .
Inductive and (A B : Prop) : Prop := and : A -> B -> and A B.
```
