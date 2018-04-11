---
title: Abdullah research roadmap
date: 2018-04-11 03:50 +0700
permalink: /abdullah.html
---

Abdullah wants to make a monad-aware programming language.

The plan is to research two related things in parallel:

- the relationship among laziness, strictness, totality, and monads
    - [McBride2015]
- using algebraic subtyping to mix parametric subtyping and inheritance subtyping
    - [Stephen Dolan's Ph.D. thesis "Algebraic subtyping"](https://www.cl.cam.ac.uk/~sd601/thesis.pdf)
        - "Type systems which support subtyping care about the direction of data flow."
        - "Find the *simplest* algebra of types, and *some* syntax for them"
    - [Wikipedia: Subtyping](https://en.wikipedia.org/wiki/Subtyping)

Scala already tries to join parametric subtyping and inheritance subtyping.
What is the problem with Scala?

See also [functional programming research]({% link functional_programming.md %}).

## Results

The folder [abdullah-conjecture](https://github.com/Lambda-Jakarta/research/tree/master/abdullah-conjecture)
contains a proposed partial proof of the Abdullah conjecture for all Haskell 98 type endofunctions.
The proof can be checked by the Lean theorem prover version 3.
See also the [Lean prover home page](https://leanprover.github.io/).
To edit Lean source files, use Visual Studio Code and its Lean plugin.
