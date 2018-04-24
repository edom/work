---
title: Abdullah research roadmap
date: 2018-04-11 03:50 +0700
permalink: /abdullah.html
mathjax: yes
---

- Abbreviations:
    - CCC: Cartesian closed category ([Wikipedia](https://en.wikipedia.org/wiki/Cartesian_closed_category))
- Abdullah wants to make a monad-aware programming language.
    - Categories enable us to organize a hierarchy of effects?
        - effectful over category \\( C \\) = extends category \\( C \\)?
- The plan is to research two related things in parallel:
    - using algebraic subtyping to mix parametric subtyping and inheritance subtyping
        - [Stephen Dolan's Ph.D. thesis "Algebraic subtyping"](https://www.cl.cam.ac.uk/~sd601/thesis.pdf)
            - "Type systems which support subtyping care about the direction of data flow."
            - "Find the *simplest* algebra of types, and *some* syntax for them"
        - [Wikipedia: Subtyping](https://en.wikipedia.org/wiki/Subtyping)
        - Scala already tries to join parametric subtyping and inheritance subtyping.
        What is the problem with Scala?
- Related: [functional programming research]({% link functional_programming.md %}).

## Research questions

### Possible questions

- What is the result of CPS-transforming a recursive function?

```haskell
fac 0 = 1
fac n = n * fac (n - 1)

fac 0 k = k 1
fac n k = fac (n - 1) $ \ x -> k (n * x)
```

Conjecture:
Every recursive function can be transformed to a tail-recursive function with a helper function \\( f(x) = f(g(x)) \\).

```haskell
fac 0 = 1
fac n = n * fac (n - 1)

fach (a, 0) = (a, 0)
fach (a, n) = fach (a * n, n - 1)

fac n = fach (1, n)
```

How do we enable the caller to step the recursion?

```haskell
fac n = \ k -> k (\ x -> x * fac (n - 1) k) n
fac n (\ f x -> f x)

tri 0 = 0
tri n = n + tri (n - 1)

tri :: Nat -> ((x -> c -> Nat) -> Nat -> Nat)
tri 0 = \ k -> 0
tri n = \ k -> k (\ x c -> x + tri (n - 1) c) n

inc x = inc (x + 1)

inc x = \ k -> k (\ ) (x + 1)
```

- [github: dorchard/unfix: Takes a recursive function and syntactically unties the recursive knot](https://github.com/dorchard/unfix)

### What is the relationship between self-reference, recursion, and fixed points?

We say that \\( x \\) is a *fixed point* of \\( f \\) iff \\( f(x) = x \\).

[MO 126513: categories of recursive functions](https://mathoverflow.net/questions/126513/categories-of-recursive-functions)

- What is the essence of self-recursion?
    - `fix` does not exist in a strict language.
        - "The Z combinator will work in strict languages [...]" [WP: Fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator)
            - The Z combinator is obtained by eta-expanding the Y combinator.

### What is the formal definition of strict, non-strict, eager, and lazy?

The difference is explained by luqui on [SO 7140978](https://stackoverflow.com/questions/7140978/haskell-how-does-non-strict-and-lazy-differ).
- Strict and non-strict are about meaning (denotational semantics?).
Eager and lazy are about operation (operational semantics?).
- Strictness is a domain-theoretic concept.
Laziness is a computer implementation detail.
- This uses Haskell to introduce domain theory: [Wikibooks: Haskell: Denotational semantics](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics).
    - In Haskell, the least fixed point operator can be defined as `fix f = f (fix f)`.
        - Why is bottom the *least* fixed point of `id`?
        Every \\( x \\) is a fixed point of an identity function \\( x \mapsto x \\), isn't it?
            - What is the ordering?
                - "Semantic approximation order"
    - [Haskell wiki](https://wiki.haskell.org/Lazy_vs._non-strict) is wrong?
    It conflates non-strictness with normal-order reduction strategy?
    - [A simple example of denotational semantics using a language of binary numerals](http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/6.DENOTATIONAL-SEMANTICS.html#simple)
    - [WP: Binary combinatory logic](https://en.wikipedia.org/wiki/Binary_combinatory_logic).
    Its semantics is SK calculus (SKI calculus without the redundant I combinator) which is equivalent to lambda calculus.
- we can execute non-strict functions eagerly,
for example by strictness analysis or speculative execution.

People are often sloppy with these terms. Redditors. Experts. Researchers. Academics.
It is true that Haskell is non-strict.
It is true that Haskell (as implemented by GHC) is lazy.

We can infer these formal definitions:
- A function \\( f \\) is *strict* iff \\( f(\bot) = \bot \\).
    - "a strict function must map bottom to bottom" (from the SO answer)

### How do we represent general recursion by a monad? How do we add general recursion to TFP? How do we do it with monads?

Here we try to salvage [McBride2015].

TODO write the problem: how McBride's General doesn't compose

- Is McBride's General really a monad?
- Is Abdullah's M really a monad?
- Did Abdullah mistranslate McBride's General?
- Is there a way to transform begin-step-end to McBride's General or Abdullah's M?
- Start with axioms, then infer the data types.

These are the axioms that we want M to satisfy.
- `rec f . rec g = rec (ext f . g)`
- `rec f . rec g = rec (f <=< g)`
- `rec pure = id`

How do we translate a recursive function `f : a -> b`
to an explicitly recursive function `f : a -> m b`?

```haskell
-- Abdullah's M, obtained by translating
-- the General in [McBride2015] from Agda to Haskell
data M s t a
    = Em a
    | Ap (t -> M s t a) s

cata :: (a -> r) -> ((t -> r) -> s -> r) -> r
cata ar xrxr m = fix $ \ self m -> case m of
    Em a -> ar a
    Ap xma x -> xrxr (self . xma) x
```

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

## Result of meeting on 2018-04-21

- https://mvanier.livejournal.com/2897.html
- Applicative Functor is a homomorphism over CCC (Cartesian closed category)?
- We can use a category as the denotation of a functional programming language.
    - An example of a category:
        - One object: Unit
        - One morphism:
        - Two functions:
            - `f0 x = Unit`
            - `f1 x = x`
- What is a CCC? It is a category that satisfies the axioms in [WP: CCC](https://en.wikipedia.org/wiki/Cartesian_closed_category).

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

### I think we can't add a Lazy Monad instance to Strict Haskell without changing the language semantics

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

## Results

### Is continuation the mother of all monads?

Abdullah wants to prove that continuation is the mother of all monads.

I think I have a partial proof of that for all Haskell-98 type endofunctions.

The folder [abdullah-conjecture](https://github.com/Lambda-Jakarta/research/tree/master/abdullah-conjecture)
contains a proposed partial proof of the Abdullah conjecture for all Haskell 98 type endofunctions.
The proof can be checked by the Lean theorem prover version 3.
See also the [Lean prover home page](https://leanprover.github.io/).
To edit Lean source files, use Visual Studio Code and its Lean plugin.

## Reading triage

[1995, D. A. Turner, Elementary Strong Functional Programming](https://pdfs.semanticscholar.org/b60b/1c2e49ec6f574f220f162c8fdc81b2831830.pdf)

[Godel's System T revisited](https://nms.kcl.ac.uk/maribel.fernandez/papers/TCS10.pdf)

[Total Functional Programming in a Partial Impure Language](http://semantic-domain.blogspot.co.id/2012/12/total-functional-programming-in-partial.html)

[Type theory and functional programming](http://www.cse.chalmers.se/~coquand/bengt.pdf):
Can we see type theory as a functional programming language?

[Thierry Coquand page at Chalmers](http://www.cse.chalmers.se/~coquand/)

[MO 126513: Categories of recursive functions](https://mathoverflow.net/questions/126513/categories-of-recursive-functions)

[Denotational semantics and rewrite rules for FP](https://www.researchgate.net/publication/234808984_Denotational_semantics_and_rewrite_rules_for_FP):
"We consider languages whose operational semantics is given by a set of rewrite rules."

[allisons.org: Denotational Semantics](http://www.allisons.org/ll/Semantics/)

The Y-combinator is \\( \lambda f. (\lambda x. f ~ (x ~ x)) ~ (\lambda x. f ~ (x ~ x)) \\).
[WP: Fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator)

[Simple Denotational Semantics for the Lambda Calculus, Pω Revisited?](http://siek.blogspot.co.id/2016/12/simple-denotational-semantics-for.html)
