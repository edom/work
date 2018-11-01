---
title: Functional programming research
date: 2018-04-07 00:00 +0700
permalink: /functional_programming.html
mathjax: yes
---

# Functional programming research

- Abbreviations
    - CPS: continuation-passing style
    - FP: functional programming
    - OS: operating system
    - PFP: partial functional programming
    - TFP: total functional programming
    - TFPL: total functional programming language
    - TM: Turing machine

## Research questions

## Monads, lazy, strict

- Can you sell me some TFP?
    - What is TFP?
        - TFP is functional programming with only total functions.
            - A function is total iff it is defined for every element of its domain.
        - TFP ensures that every function is total by constraining every recursion to be structural recursion.
            - Structural recursion is recursion with the constraint that every recursive call is syntactically smaller.
    - Why TFP?
        - The practical advantage of TFP is that it is easier
        (and therefore faster and less costly)
        to write provably correct programs in TFP
        compared to PFP or imperative programming.
            - This ultimately translates to faster time-to-market,
            lower development cost, fewer errors, fewer customer complaints, and higher customer satisfaction.
            (I know this sounds like bullshit. We should not underestimate the ways that humans can screw up.)
        - The theoretical advantage of TFP
        is that TFP has simpler denotational semantics compared to PFP
        because TFP has less bottoms to consider. [Turner2004]
    - Why not TFP?
        - A disadvantage of TFP is that it is not Turing-complete because all programs must terminate,
        but OSes don't terminate,
        but we can still write an OS in a TFPL using codata and corecursion [Turner2004]
        which are discussed in another section in this document.

# Ramble

**You should not read anything below this point.**
These are the drafts of my drafts.
I think they should be included in this page,
but I haven't worked them enough.

- Why did Simon Peyton-Jones write that the next Haskell will be strict?
    - [Simon Peyton-Jones PPT slides](http://www.cs.nott.ac.uk/~gmh/appsem-slides/peytonjones.ppt).
    - [ycombinator comment thread](https://news.ycombinator.com/item?id=1924061).

This is a questionnaire, not survey article:
A preliminary survey of functional programming
Caitlin Sadowski
Daan Leijen
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/paper-67.pdf

- How do these differ: Agda, Coq, Lean, Isabelle, ACL2, and others?
    - [Conor McBride's tangential opinions](https://www.reddit.com/r/haskell/comments/3b498l/if_you_could_change_one_thing_about_haskell_what/csk2gvl/)
    on Coq vs Agda vs others
    - [Agda Wiki: Agda vs Coq](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=Main.AgdaVsCoq)
    - [/r/haskell: Lean: the new open source theorem prover developed at Microsoft Research](https://www.reddit.com/r/haskell/comments/30j9l6/lean_the_new_open_source_theorem_prover_developed/)

## Lambda calculus

- History
    - Alonzo Church introduced lambda calculus in 1932 in [Church1932].
    - Notations have changed.
    Church wrote \\( S_y^x U \\) but we write \\( U[x := y] \\) for the same thing:
    the expression \\(U\\) but with every occurrence of free variable \\(x\\) replaced by expression \\(y\\).
    - [Church1932] credits a 1924 publication of Moses Schönfinkel for what we call *currying*:
    changing a multi-parameter function to a one-parameter function
    returning another one-parameter function returning yet another one-parameter function, and so on.
    Currying is changing \\( f : (a,b) \to c \\) to \\( f' : a \to (b \to c) \\) such that \\( f(x,y) = (f'(x))(y) \\).
- See also [Wikipedia: Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

### might be too old

- [Lof1984]
- 1986 A survey of functional programming language principles https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19870002073.pdf

## Codata, corecursion, and coinduction

For more about codata, corecursion, and coinduction, see:
- ["Corecursion and coinduction: what they are and how they relate to recursion and induction", Mike Gordon](http://www.cl.cam.ac.uk/archive/mjcg/plans/Coinduction.html):
"My goal here is to try to understand these things through the activity of creating a simple explanation."
- ["Data and Codata", Dan Piponi](http://blog.sigfpe.com/2007/07/data-and-codata.html):
"The program might not terminate, but from a mathematical perspective this is a completely well defined function."
"Note the duality: in structural recursion we 'deconstruct' the argument and then we're allowed to recurse. In guarded recursion we recurse first, and then we're allowed to use the constructor."
- ["Data vs Codata", Michael Maloney](https://www.tac-tics.net/blog/data-vs-codata)

## Miscellany

[Turner2004] calls TFP "strong functional programming".

## Lazy, strict

- The Church-Rosser property
([Wikipedia](https://en.wikipedia.org/wiki/Church%E2%80%93Rosser_theorem),
[Mathworld](http://mathworld.wolfram.com/Church-RosserProperty.html)),
eliminates the difference between strict and lazy in a TFPL [Turner2004].

## Type theory is a formal system, not a branch of mathematics

There are at least two type theories:
the Martin-L\"of type theory, and the calculus of constructions [Bove2001].
I thought "type theory" was a branch of mathematics,
like "category theory", "graph theory", and "number theory".

"Martin-L\"of's type theory is basically a theory about sets in which it is possible to interpret a logic." [Nordstrom1988]

## Recursion and fixpoint

We say that \\( x \\) is a *fixpoint* of \\( f \\) iff \\( x = f(x) \\).

A [text lecture](https://www.cs.cornell.edu/courses/cs3110/2013sp/supplemental/lectures/lec29-fixpoints/lec29.html)
describes how to obtain the factorial function as a fixed point of successive *approximations*.

[Wikipedia: Fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator).
A fixed-point combinator enables *anonymous* recursive functions.

TFP rejects the definition `fix f = f (fix f)` because this is not a structural recursion.

Recursion is about fixpoint.

## The Eff language and monad-aware languages

Consider this passage from [McBride2015]:

> [The Eff language lets] us write in direct
style for whatever effectful interface is locally available, then obtain the computation
delivered by the appropriate Moggi-style translation into an explicitly monadic
kernel.

I think "in direct style"
means that in Eff we can write `f x` to mean what we would write as `x >>= f` in Haskell,
but with the Monad instance automatically inferred based on the locally available effects.

I think that passage suggests that the Eff language [Bauer2012]
is related to the "monad-aware language" that Abdullah is trying to accomplish.
The relation is that Eff infers the monad from the locally available effects.
However, Eff's type system ignores the effects (like ML's type system)
so this is probably not what Abdullah wants.

## Reading triage

http://semantic-domain.blogspot.co.id/2016/03/agda-is-not-purely-functional-language.html

https://en.wikipedia.org/wiki/Category:Term-rewriting_programming_languages

In a term-rewriting language such as Pure, we can write program transformation as part of the program.
https://stackoverflow.com/questions/24330902/how-does-term-rewriting-based-evaluation-work

https://www.quora.com/What-are-examples-of-statically-typed-logic-programming-languages-ex-similar-to-Prolog

https://mathoverflow.net/questions/3920/what-does-it-mean-to-discharge-assumptions-or-premises

http://www.cs.nott.ac.uk/~pszvc/g54dtp/inductive_domain.v

https://stackoverflow.com/questions/145263/what-is-total-functional-programming

What is a computational effect?

Does totality really have anything to do with termination?

## TFP and Turing-completeness

We must distinguish between a Turing machine and its execution.

A TM needs a tape to run, but does not come with it.
To run a TM, you have to supply a tape.

A *tape* is a finite sequence of tape symbols.

A *step* is a pair of configuration and tape.

A *run* is a sequence of steps. This sequence may be infinite.

A *run* of a Turing machine \\( m \\) with initial tape \\( t \\) is ...

Instead of thinking about infinitely long tape with infinitely many blank symbols,
we think that the tape is finite but the TM may at every step
insert, update, or delete a cell.

A TM is finite by definition: a TM is a tuple whose each component is taken from a finite set.
The description of a TM does not include the tape.
The execution trace of a TM may be infinite.
A TFPL can *describe* every TM just fine.

We can describe a Turing machine in a TFPL?

https://www.reddit.com/r/programming/comments/jvu2w/total_functional_programming_and_the_unimportance/

[LTU: Total functional programming](http://lambda-the-ultimate.org/node/2003)

https://news.ycombinator.com/item?id=12646390

https://existentialtype.wordpress.com/2014/03/20/old-neglected-theorems-are-still-theorems/

https://math.stackexchange.com/questions/111773/are-total-recursive-functions-recursively-enumerable

How do we write this "echo" program in TFP? Is it even total?

```haskell
main : IO ()
main = getLine >>= putStrLn >> main
```

We can *describe* the infinite list of natural numbers `[0,1,2,3,...]` in a TFP.

```
f : Nat -> Nat
f n = n + 1

f : Nat -> Nat
f n = n
```

[Partiality, Revisited: The Partiality Monad as a Quotient Inductive-Inductive Type](https://arxiv.org/abs/1610.09254)

http://www.cl.cam.ac.uk/archive/mjcg/plans/Coinduction.html

https://en.wikipedia.org/wiki/Natural_transformation

https://en.wikipedia.org/wiki/Initial_algebra
endofunctor, F-algebra, initial object

[probabilistic logic programming](https://dtai.cs.kuleuven.be/problog/)

## Metaprogramming

Spoofax vs Xtext vs MPS?
http://www.metaborg.org/en/latest/
http://www.metaborg.org/en/latest/source/overview/examples.html

PEG (parsing expression grammar)

http://ttic.uchicago.edu/~dmcallester/foundations.pdf
https://en.wikipedia.org/wiki/Foundations_of_mathematics
https://web.stanford.edu/class/cs103/notes/Mathematical%20Foundations%20of%20Computing.pdf
https://github.com/hoplon/javelin
https://codon.com/consider-static-typing

## Structured editor

https://news.ycombinator.com/item?id=13773813

https://www.reddit.com/r/programming/comments/1tp83j/lamdu_structuralast_editor/

## Others

[Compile and run Agda programs online](https://tio.run/#agda).

## Positive and negative positions, strict positivity

In the function type \\( A \to B \\),
we say that \\( A \\) occurs in a *negative* position
and \\( B \\) occurs in a *positive* position.
See [CS StackExchange 42150](https://cs.stackexchange.com/questions/42150/meaning-of-positive-position-and-negative-position-in-type-theory).

For the *strict positivity* constraint of a data type definition, see these:
[CS StackExchange 55646](https://cs.stackexchange.com/questions/55646/strict-positivity),
[Agda Wiki: Strict positivity](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.SimpleInductiveTypes?from=ReferenceManual.Datatypes#Strictpositivity).

In a TFPL, there is no expression whose type is `forall a. a`.

```
exit : IO a
```

## Rewriting systems

A TFPL program is a terminating ARS.
https://en.m.wikipedia.org/wiki/Termination_(term_rewriting)

[SoftOption: normal forms and termination](https://softoption.us/content/node/37)

[SoftOption: lambda calculus and combinatory logic](https://softoption.us/content/node/654)

There are at least two widely used confluent term-rewriting systems:

- applicative, lambda calculus, beta-reduction
- concatenative, postfix notation, PostScript, Forth

An expression is in normal form iff there is no applicable rewrite rule.
See [Wikipedia: Normal form (abstract rewriting)](https://en.wikipedia.org/wiki/Normal_form_(abstract_rewriting)).

The following `hang` function is not total.
Why?

```haskell
hang : Nat -> Nat
hang x => hang x
```

The expression `hang x` can be rewritten to `hang x`, so `hang x` is not a normal form,
but this goes on forever, so `hang x` does not have a normal form, and thus `hang x` is not total.

This `loop` thing is not a function in the mathematical sense. Why is that?

```
loop : a
loop => loop
```

Consider this, where `exit` terminates the program.

```haskell
what : Nat
what => exit
```

The function `crash` also does not produce a `Nat`.

- What is Scott continuity?
    - Why is it named "continuity"?

https://cs.stackexchange.com/questions/1371/scott-continuous-functions-an-alternative-definition

Girard's System F
https://people.mpi-sws.org/~skilpat/plerg/papers/harper-system-f-2up.pdf
https://www.reddit.com/r/haskell/comments/2zqtfk/why_isnt_anyone_talking_about_optimal_lambda/

Venanzio Capretta's partiality monad
General recursion via coinductive types
[Venanzio Capretta 2005](http://www.cs.ru.nl/~venanzio/publications/Recursion_Coinductive_LMCS_2005.pdf)
"see the work by Barendregt
and Geuvers [5] for a good exposition of technical issues of type-theoretic proof assistants"
Coinductive types were first
introduced in type theory by Hagino [34]

[StackOverflow: What can Idris not do by giving up Turing-completeness?](https://cs.stackexchange.com/questions/19577/what-can-idris-not-do-by-giving-up-turing-completeness):
"Dual to termination: while an inductive definition must terminate (by consuming all of its data) a coinductive definition must be productive - in practice this means, brieflt, that any recursive call must be guarded by a constructor. I've found this explanation to be the clearest (ymmv): adam.chlipala.net/cpdt/html/Coinductive.html" – Edwin Brady Apr 22 '14 at 17:58

Dependent Types and Multi-monadic Effects in F⋆
https://www.fstar-lang.org/papers/mumon/paper.pdf

Type Systems, Luca Cardelli
http://lucacardelli.name/Papers/TypeSystems.pdf

[Karl Voelker master thesis: practical programming with total functions](https://www.cs.rit.edu/~mtf/student-resources/20094_voelker_msthesis.pdf)

[York CS wiki: FP researches](https://www.cs.york.ac.uk/plasma/wiki/index.php?title=Functional_Programming#Theses)

- Position papers
    - [2007, Hyland & Power: "The Category Theoretic Understanding of
    Universal Algebra: Lawvere Theories and
    Monads"](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/hyland-power-lawvere-theories-and-monads.pdf)

- Should we read these?
    - 2015 Ralf Hinze, Nicolas Wu, Jeremy Gibbons: [Conjugate Hylomorphisms Or: The Mother of All Structured Recursion Schemes](http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/conjugate-hylos.pdf)
    - 2007 Colin John Morris Kemp PhD thesis [Theoretical foundations for practical "totally functional programming"](https://pdfs.semanticscholar.org/21da/de9b8e96724265f911c90c0ddd935901a0f0.pdf)
