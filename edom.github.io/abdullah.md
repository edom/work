---
title: Abdullah research roadmap
date: 2018-04-11 03:50 +0700
permalink: /abdullah.html
---

- Abdullah wants to make a monad-aware programming language.
- The plan is to research two related things in parallel:
    - the relationship among laziness, strictness, totality, and monads
        - Read [McBride2015].
    - using algebraic subtyping to mix parametric subtyping and inheritance subtyping
        - [Stephen Dolan's Ph.D. thesis "Algebraic subtyping"](https://www.cl.cam.ac.uk/~sd601/thesis.pdf)
            - "Type systems which support subtyping care about the direction of data flow."
            - "Find the *simplest* algebra of types, and *some* syntax for them"
        - [Wikipedia: Subtyping](https://en.wikipedia.org/wiki/Subtyping)
        - Scala already tries to join parametric subtyping and inheritance subtyping.
        What is the problem with Scala?
- Related: [functional programming research]({% link functional_programming.md %}).

## Agenda for 2018-04-21

### Totality is not about termination

Consider this example.
This recursion is not structural.
However, it terminates under normal-order beta-reduction strategy.

```haskell
-- Constant function.
f : Nat -> Nat
f _ = 0

main : Nat
main = f main
```

Is `main` a total function?
Does that question make sense?
Note that `main` is not a mathematical function.
The denotation of `main` might be a mathematical function.

Does totality depend on the reduction strategy?
Does that question make sense?

I conjecture that every general recursive function can be transformed into its begin-step-end form.
See [Approximating general recursion in TFP]({% link tfp_gen_rec.md %}).

### I think we can't add a Lazy monad to Strict Haskell

Abdullah wrote:

> I phrased the research topic as a question: "Is laziness monadic over strict?" (One could also ask, "Is turing-complete monadic over total?")
The 3 key expressions are "laziness", "monadic over," and "strict."
As we already know, each of them is multivalued.

> Here's one way to make everything concrete: Let "strict" mean "strict haskell."
The latter doesn't have infinite lists.
So lazy haskell is monadic over strict haskell with the usual haskell monadic combinators.

> We understand and appreciate monads as today's best practice for managing such extensions.
Hence, the motivation behind this research. Many extensions have tidily organized themselved into monads. In particular, IO is monadic. What's uncharted is lazy over strict.

> Turner's Total FP paper gives good reasons as to why we want to program in a total language. But we want the cake and eat it too. We want to write programs, part of which is total and part of which isn't. And we want the language to manage the hierarchy of extensions for us. The best way we know how is via monads.

> Let's illustrate in a gedanken language with explicit extension combinators: Whereas `head :: [a] -> a` in the total fragment (i.e. the base space), we'd have `fmap head :: {a} -> a` in the extension, where `[a]` means finite lists and `{a}` means possibly infinite ones.

Here I try to (and fail to) add a Lazy monad to an imaginary language Strict Haskell (SH).

Imagine SH, a language with Haskell syntax
but with Scheme's applicative-order beta reduction (AOBR) instead of Haskell's normal-order beta reduction.
AOBR means: to evaluate `f x`, first evaluate `x`, and then evaluate `f x`.
SH is strict, and Haskell is lazy.

An inhabitant of the type `Lazy a` can be thought as a thunk that will return an inhabitant of `a`.

To construct an expression of type `Lazy a`, combine these:
- The expression `bottom` constructs a thunk will fail.
- The expression `pure x` constructs a thunk that will return `x`.
Note that `x` is evaluated before the thunk is constructed.
- The expression `delay f` constructs a thunk that will return the result of evaluating `f Unit`.
The type of `f` is `Unit -> a`.
Note that `f unit` is not evaluated when the thunk is constructed,
unlike `pure`.
- The expression `eval t` evaluates or forces the thunk `t`.
The type of `eval` is `Lazy a -> Maybe a`.

We want to embed laziness into SH.
Formally, this means that we want this equation to hold

```
eval (bottom >>= \ x -> pure c) = Just c
```

but this is impossible in SH because the `>>=` is strict.

However, if the type of `>>=` were this

```
(Monad m) => m (Lazy a) -> (Lazy a -> m (Lazy b)) -> m (Lazy b)
````

then it would be possible to embed laziness into SH.

Thus `Lazy` cannot be a `Monad` instance in SH.

Monads allow embedding a strict language in a lazy language [Wadler1996].
We are trying the reverse (embedding a lazy language in a strict language).
We have just tried the most naive way.
It failed.

### Does TFP really sacrifice Turing-completeness?

- What is a rigorous definition of Turing-completeness?
    - [Wikipedia](https://en.wikipedia.org/wiki/Turing_completeness):
    "a system of data-manipulation rules (such as a computer's instruction set, a programming language, or a cellular automaton)
    is said to be Turing complete or computationally universal if it can be used to simulate any Turing machine"
        - What does "simulate" mean?
    - What is the relationship among total Turing machine, partial Turing machine, total function, and partial function?
    See [Wikipedia: Machine that always halts](https://en.wikipedia.org/wiki/Machine_that_always_halts).
- Why do you ask this?
    - We've been thinking that totality precludes Turing-completeness, but Conor McBride disagrees in [McBride2015].

### How do we add general recursion to TFP? How do we do it with monads?

- Why do we want to add general recursion to TFP?
    - Adding general recursion to a TFPL
    makes programming in that language more practical.
- There are several attempts to add general recursion to TFP.
    - [Nordstrom1988] (terminating general recursion)
    - [Bove2001] (simple general recursion in type theory)
    - [Capretta2005] (general recursion via coinductive types)
    - [McBride2015] (Turing-completeness totally free)
    - me on 2018-04-07: [Approximating general recursion in TFP]({% link tfp_gen_rec.md %})
    - [A non-termination monad inspired by domain theory](http://adam.chlipala.net/cpdt/html/GeneralRec.html),
    part of the documentation of Coq's GeneralRec library
- How are monads useful in FP?
    - Monads allow embedding a strict language in a lazy language [Wadler1996].
    - "Monads may be regarded as a mild generalization of continuation-passing style." [Wadler1996]
- [Philip Wadler's research on monads](http://homepages.inf.ed.ac.uk/wadler/topics/monads.html)
- Reading triage:
    - Moggi 1991: Notions of computation and monads
        - Programs should form a category.
            - Every type becomes an object in the category.
            - Every (one-parameter) function becomes a morphism in the category.
        - "Kleisli triples are just an alternative description for monads. Although
        the former are easy to justify from a computational perspective, the latter
        are more widely used in the literature on category theory and have the
        advantage of being defined only in terms of functors and natural transformations,
        which make them more suitable for abstract manipulation."
        - Moggi's most cited paper, according to Google Scholar
        - [Moggi's home page](https://www.disi.unige.it/person/MoggiE/)
            - [Moggi's list of his publications](https://www.disi.unige.it/person/MoggiE/publications.html)
                - recent paper: 2010 "Monad Transformers as Monoid Transformers". Theoretical Computer Science, TCS vol.411
    - [Moggi 1989: Computational lambda-calculus and monads](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/moggi-computational-lambda-calculus-and-monads.pdf)
    - 2017 Uustalu [Partiality and container monads](https://www.semanticscholar.org/paper/Partiality-and-Container-Monads-Uustalu-Veltri/a45cabd8696232a985368e5b7f138fd21a7bff9f)
    - [Sheard2003] (a pure language with default strict evaluation order and explicit laziness)
    - [Wadler1998] (how to add laziness to a strict language without even being odd)
    - [Wadler1992] "explores the use of monads to structure functional programs"
    - [Monad for lazy evaluation](https://srfi.schemers.org/srfi-40/mail-archive/msg00059.html),
    Scheme, SRFI-40 mail archive, Andre van Tonder
    - [Not all computational effects are monads](http://math.andrej.com/2008/11/17/not-all-computational-effects-are-monads/)
    - 2018 Tomas Petricek [What we talk about when we talk about monads](https://arxiv.org/pdf/1803.10195.pdf)

## Results

### Is continuation the mother of all monads?

Abdullah wants to prove that continuation is the mother of all monads.

I think I have a partial proof of that for all Haskell-98 type endofunctions.

The folder [abdullah-conjecture](https://github.com/Lambda-Jakarta/research/tree/master/abdullah-conjecture)
contains a proposed partial proof of the Abdullah conjecture for all Haskell 98 type endofunctions.
The proof can be checked by the Lean theorem prover version 3.
See also the [Lean prover home page](https://leanprover.github.io/).
To edit Lean source files, use Visual Studio Code and its Lean plugin.
