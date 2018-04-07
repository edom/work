---
title: Functional programming research
date: 2018-04-07 00:00 +0700
permalink: /function.html
mathjax: yes
---

## Abbreviations

CPS: continuation-passing style.

PFP: partial functional programming.

TFP: total functional programming.

TFPL: total functional programming language.

OS: operating system.

## Adding general recursion to TFP

Why do we want to add general recursion to TFP?
Because, adding general recursion to a TFPL
makes programming in that language more practical.

So far we think that totality implies the absolute prohibition of general recursion
(and therefore sacrificing Turing-completeness),
but there are several attempts to add general recursion to TFP.

Reading triage:

- [Nordstrom1988] (terminating general recursion)
- [Bove2001] (simple general recursion in type theory)
- [Capretta2005] (general recursion via coinductive types)
- [A non-termination monad inspired by domain theory](http://adam.chlipala.net/cpdt/html/GeneralRec.html),
part of the documentation of Coq's GeneralRec library
- [McBride2015] (Turing-completeness totally free)

## Stop abusing the equal sign

The equal sign should be used for equations only and nothing else.

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

## Function

A *relation* is a triple \\( (A,B,R) \\) where
\\( A \\) is a set called the *domain*,
\\( B \\) is a set called the *codomain*,
and \\( R \subseteq A \times B \\) is a set that is the relation's *mapping*.

A relation is not simply a subset of a Cartesian product.
The domains and codomains matter.

A *function* is a relation in which each element of the domain
appear exactly once in the mapping (on the left of the ordered pair).
A function must map everything in its domain.

Let \\( f = (A,B,F) \\) be a function.
The notation of "applying \\( f \\) to \\( x \\)", written \\( f(x) \\),
means the \\( y \\) such that \\( (x,y) \in F \\).

A *partial function* from \\( A \\) to \\( B \\) is a function from \\( A \\) to \\( B_\bot \\)
where \\( B_\bot = B \cup \\{ \bot \\} \\).
The symbol \\( \bot \\) is called *bottom*.
We require that \\( \bot \not\in B \\).

What is the difference between "undefined" and "bottom"?

Every partial function can be made total by shrinking the domain.
Every total function can be made partial by expanding the domain.
Why do we care about the difference?
Because the Curry-Howard isomorphism implies that total functions are sound?
But how about proof irrelevance?

## Function equality

Two relations are *equal* iff their domains are equal,
their codomains are equal,
and their mappings are equal.
Formally, \\( (A_1,B_1,R_1) = (A_2,B_2,R_2) \\) iff \\( A_1 = A_2 \\) and \\( B_1 = B_2 \\) and \\( R_1 = R_2 \\).

Functions are relations.
Equality of functions is equality of relations.

These functions `idn` and `idz` are *different* functions.

```haskell
idn : Nat -> Nat
idn x => x

idz : Int -> Int
idz x => x
```

This `loop` thing is not a function. Why is that?

```
loop : a
loop => loop
```

In a TFPL, there is no expression whose type is `forall a. a`.

```
exit : IO a
```

A TFPL program is a terminating ARS.
https://en.m.wikipedia.org/wiki/Termination_(term_rewriting)

[SoftOption: normal forms and termination](https://softoption.us/content/node/37)

[SoftOption: lambda calculus and combinatory logic](https://softoption.us/content/node/654)

There are at least two widely used confluent term-rewriting systems:

- applicative, lambda calculus, beta-reduction
- concatenative, postfix notation, PostScript, Forth


[Agda Wiki: Totality](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.Totality)

[ncatlab: partial function](https://ncatlab.org/nlab/show/partial+function)

The function \\( f : A \to B \\) has input type \\( A \\) and output type \\( B \\).

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

Consider this, where `exit` terminates the program.

```haskell
what : Nat
what => exit
```

The function `crash` also does not produce a `Nat`.

What is Scott continuity?
Why is it named "continuity"?

https://cs.stackexchange.com/questions/1371/scott-continuous-functions-an-alternative-definition

https://en.wikipedia.org/wiki/Turing_completeness

Partial Turing machine, total Turing machine.
What is the significance of the theorem in [Wikipedia: Total Turing machine](https://en.wikipedia.org/wiki/Total_Turing_machine)?

What is the relationship between total function and total Turing machine?

## Positive and negative positions, strict positivity

In the function type \\( A \to B \\),
we say that \\( A \\) occurs in a *negative* position
and \\( B \\) occurs in a *positive* position.
See [CS StackExchange 42150](https://cs.stackexchange.com/questions/42150/meaning-of-positive-position-and-negative-position-in-type-theory).

For *strict positivity* of a data type, see these:
[CS StackExchange 55646](https://cs.stackexchange.com/questions/55646/strict-positivity),
[Agda Wiki: Strict positivity](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.SimpleInductiveTypes?from=ReferenceManual.Datatypes#Strictpositivity).

[Compile and run Agda programs online](https://tio.run/#agda).

## Approximating general recursion in TFP

Here we are going to show how to approximate general recursive functions in TFP
with the help of the following `repeat` and `approx`:

```haskell
-- The expression `repeat n f x` means
-- `n` times of the application of `f` to `x`.
repeat : Nat -> (a -> a) -> a -> a
repeat 0 f = id
repeat (n+1) f = repeat n f . f

approx
    : Nat -- count
    -> (a -> s) -- begin
    -> (s -> s) -- step
    -> (s -> Maybe b) -- end
    -> (a -> Maybe b)

approx count begin step end =
    end . repeat count step . begin
```

To approximate a general recursive function `f : a -> b`,
we write a data type `S_f` and these three non-recursive total functions:

```haskell
begin_f : a -> S_f
step_f : S_f -> S_f
end_f : S_f -> Maybe b

-- A side note:
-- In PFP, the original `f` can be
-- recovered from those three functions:
f input = loop (begin_f input)
    where
        loop s = case end_f s of
            Just output -> output
            _ -> loop (step_f s)
```

Then, we can approximate `f` as `f_approx`:

```haskell
f_approx : Nat -> (a -> Maybe b)
f_approx count =
    approx count begin_f step_f end_f
```

The `count` parameter can be thought as a time limit or time-out,
the number of iterations,
the number of steps.

Here is an example approximation of the factorial function.

```haskell
fac 0 = 1
fac n = n * fac (n-1)

data State = Mk { n : Nat, a : Nat }

fac_approx count =
    end . repeat count step . begin
    where
        begin : Nat -> State
        begin n = Mk n 1

        end : State -> Maybe Nat
        end (Mk 0 a) = Just a
        end _ = Nothing

        step : State -> State
        step (Mk 0 a) = Mk 0 a
        step (Mk (n+1) a) = Mk n (a * (n+1))
```

Here is an example approximation of bottom.

```haskell
-- PFP

hang : a
hang = hang

-- TFP approximation

data State = Mk

hang_begin _ = Mk
hang_step s = s
hang_end _ = Nothing

hang_approx count =
    hang_end . repeat count hang_step . hang_begin
```

I conjecture that there is an algorithm that can transform every general recursive function into its begin-step-end form.

## Monads, lazy, strict

Monads allow embedding a strict language in a lazy language [Wadler1996].

"Monads may be regarded as a mild generalization of continuation-passing style."
[Wadler1996]

Tomas Petricek
2018
What we talk about when we talk about monads
https://arxiv.org/pdf/1803.10195.pdf

Conjugate Hylomorphisms
Or: The Mother of All Structured Recursion Schemes
Ralf Hinze Nicolas Wu Jeremy Gibbons
2015
http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/conjugate-hylos.pdf

Reading triage:

- 2017 https://www.semanticscholar.org/paper/Partiality-and-Container-Monads-Uustalu-Veltri/a45cabd8696232a985368e5b7f138fd21a7bff9f
- [Sheard2003] (a pure language with default strict evaluation order and explicit laziness)
- [Wadler1998] (how to add laziness to a strict language without even being odd)
- [Wadler1992] "explores the use of monads to structure functional programs"
- [Monad for lazy evaluation](https://srfi.schemers.org/srfi-40/mail-archive/msg00059.html),
Scheme, SRFI-40 mail archive, Andre van Tonder
- [Not all computational effects are monads](http://math.andrej.com/2008/11/17/not-all-computational-effects-are-monads/)

# Ramble

**You should not read anything below this point.**
These are the drafts of my drafts.
I think they should be included in this page,
but I haven't worked them enough.

## What is TFP? Why? Why not?

TFP is functional programming with only total functions.
A function is total iff it is defined for every element of its domain.
TFP constrains every recursion to be structural recursion.
Structural recursion is recursion with the constraint that every recursive call is syntactically smaller.

The practical advantage of TFP is that it is easier
(and therefore faster and less costly)
to write provably correct programs in TFP
compared to PFP or imperative programming.
This ultimately translates to faster time-to-market,
lower development cost, fewer errors, fewer customer complaints, and higher customer satisfaction.

The theoretical advantage of TFP
is that TFP has simpler denotational semantics compared to PFP
because TFP has less bottoms to consider,
but a disadvantage of TFP is that it is not Turing-complete because all programs must terminate,
but OSes don't terminate,
but we can still write an OS in a TFPL using codata and corecursion [Turner2004]
which are discussed in another section in this document.

## Reading triage

Theoretical foundations for practical "totally functional programming"
Colin John Morris Kemp 
2007
PhD thesis
https://pdfs.semanticscholar.org/21da/de9b8e96724265f911c90c0ddd935901a0f0.pdf

Why will "the next Haskell be strict"?
[Simon Peyton-Jones PPT slides](http://www.cs.nott.ac.uk/~gmh/appsem-slides/peytonjones.ppt).
[ycombinator comment thread](https://news.ycombinator.com/item?id=1924061).

This is a questionnaire, not survey article:
A preliminary survey of functional programming
Caitlin Sadowski
Daan Leijen
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/paper-67.pdf

An Algorithm for Optimal Lambda Calculus Reduction, John Lamping
http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.2386&rep=rep1&type=pdf

http://www.lsv.fr/Publis/PAPERS/PDF/sinot-wrs07.pdf
Complete Laziness: a Natural Semantics, François-Régis Sinot

http://rochel.info/
graph-rewriting-lambdascope (screenshot): An implementation of an optimal evaluator for the λ-calculus, PDFLambdascope

["The lambda calculus is algebraic", Peter Selinger](https://www.mscs.dal.ca/~selinger/papers/combinatory.pdf):
"We argue that free variables should not be interpreted as elements in a model, as is usually done, but as indeterminates."

["On the algebraic models of lambda calculus", Antonino Salibra](https://pdfs.semanticscholar.org/055d/69ee4dc95fbf6457419c90338493667478b1.pdf):
"The variety (equational class) of lambda abstraction algebras was introduced
to algebraize the untyped lambda calculus in the same way Boolean algebras algebraize the classical propositional calculus."
Propositional logic is modeled by Boolean algebra.
First-order logic is modeled by cylindric algebra?
Lambda calculus is modeled by lambda abstraction algebra.
Why algebra? Because it is equational?
[Wikipedia "algebraic logic"](https://en.wikipedia.org/wiki/Algebraic_logic)

["The algebraic lambda-calculus", Lionel Vaux](https://pdfs.semanticscholar.org/7596/19f05a42ff3045bcf87fcaa3edbff01e1130.pdf)

["Lambda abstraction algebras: representation theorems", Don Pigozzi, Antonino Salibra](https://pdfs.semanticscholar.org/44c9/2ad00b8ceba78319005db048b24d61a80748.pdf):

["Applying Universal Algebra to Lambda Calculus", Giulio Manzonetto, Antonino Salibra](http://www.dsi.unive.it/~salibra/mainfinale.pdf)

Dana Scott's PCF; also search the Internet for "the language pcf"
["Introduction to Real PCF (Notes)", Mart\'in H\"otzel Escard\'o](http://www.cs.bham.ac.uk/~mhe/papers/RNC3.pdf)

How do these differ: Agda, Coq, Lean, Isabelle, ACL2, and others?

coq vs agda vs whatnot
https://www.reddit.com/r/haskell/comments/3b498l/if_you_could_change_one_thing_about_haskell_what/csk2gvl/

[Agda Wiki: Agda vs Coq](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=Main.AgdaVsCoq)

[/r/haskell: Lean: the new open source theorem prover developed at Microsoft Research](https://www.reddit.com/r/haskell/comments/30j9l6/lean_the_new_open_source_theorem_prover_developed/)

Lazy imperative programming
John Launchbury
1993
https://pdfs.semanticscholar.org/492b/200419199892857faa6a6956614641ae9464.pdf

## Lambda calculus

Lambda calculus was introduced in [Church1932].

Notations have changed.
Church wrote \\( S_y^x U \\) but we write \\( U[x := y] \\) for the same thing:
the expression \\(U\\) but with every occurrence of free variable \\(x\\) replaced by expression \\(y\\).

[Church1932] credits a 1924 publication of Sch\"onfinkel for what we call *currying*:
changing a multi-parameter function to a one-parameter function
returning another one-parameter function returning yet another one-parameter function, and so on.
Currying is changing \\( f : (a,b) \to c \\) to \\( f' : a \to (b \to c) \\) such that \\( f(x,y) = (f'(x))(y) \\).

See also [Wikipedia: Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

### might be too old

[Lof1984]

1986
A survey of functional programming language principles
https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19870002073.pdf

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

https://en.wikipedia.org/wiki/Strict_programming_language

https://en.wikipedia.org/wiki/Lazy_evaluation

[Strict-by-default vs Lazy-by-default](https://memo.barrucadu.co.uk/strict-vs-lazy.html)

https://en.wikipedia.org/wiki/Applicative_computing_systems

non-strict, beta reduction, normal order, applicative order

The terms "lazy" and "strict" imply operational semantics.
They are two strategies for beta-reduction.
"Lazy" is normal-order.
"Strict" is applicative-order.

Abdullah hinted that BUBS (bottom-up beta-substitution [Shivers2004])
might be used to make a garbage-free or a fast interpreter.

The Church-Rosser property
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

Does totality really have anything to do with termination?

Something is *Turing-complete* iff it can simulate every Turing machine?

We can describe a Turing machine in a TFPL?

https://www.reddit.com/r/programming/comments/jvu2w/total_functional_programming_and_the_unimportance/

https://news.ycombinator.com/item?id=12646390

https://existentialtype.wordpress.com/2014/03/20/old-neglected-theorems-are-still-theorems/

https://math.stackexchange.com/questions/111773/are-total-recursive-functions-recursively-enumerable


Is this program total? The program's recursion is not structural. However, it terminates if we use normal-order beta-reduction strategy.

```haskell
-- Constant function.
f : Nat -> Nat
f _ => 0

main : Nat
main => f main
```

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
