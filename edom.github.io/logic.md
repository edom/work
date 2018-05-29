---
title: Logic
permalink: /logic.html
date: 2017-07-02 16:00:00 +0700
mathjax: yes
---

- Let's study a *propositional calculus*, a formal language.
    - It's important because we're going to build other formal languages on it.
    - A "calculus" is a set of rules.
        - What is the "calculus" in "propositional calculus"?
            - [EOM:Calculus](https://www.encyclopediaofmath.org/index.php/Calculus)
    - [WP:Propositional calculus](https://en.wikipedia.org/wiki/Propositional_calculus)
- A *language* has *syntax* (form) and *semantics* (meaning).
- A *formal language* describes a set of strings by using an *alphabet* and some *formation rules*.
    - The alphabet is the set of symbols that can be in the strings.
    - The formation rules decide which strings are in the language.
- A *formula* is a string in the language.
- For a propositional calculus:
    - The alphabet is:
        - the set of these five blackboard-bold *logical symbols*:
            - \\( \bbN \\) (negation, "not"),
            - \\( \bbC \\) (conjunction, "and"),
            - \\( \bbD \\) (disjunction, "or"),
            - the left parenthesis,
            - and the right parenthesis;
        - plus a set of *non-logical symbols*:
            - Anything can be non-logical symbols as long as it isn't already a logical symbol.
            - Usually a non-logical symbol is a Latin capital letter,
                - but we don't have to restrict ourselves to one letter per symbol.
                    - We can freely decide that a symbol can be one word, or even a phrase.
                        - It may even be outside the Latin alphabet.
                        - It might be a Chinese character, or an emoticon, or a drawing.
    - The formation rules are:
        1. Every non-logical symbol alone is a formula.
        1. If \\(\alpha\\) is a formula,
        then \\((\bbN ~ \alpha)\\) ("not \\(\alpha\\)") is a formula.
        1. If \\(\alpha\\) is a formula and \\(\beta\\) is a formula,
        then \\((\bbC ~ \alpha ~ \beta)\\) ("\\(\alpha\\) and \\(\beta\\)") is a formula.
        1. If \\(\alpha\\) is a formula and \\(\beta\\) is a formula,
        then \\((\bbD ~ \alpha ~ \beta)\\) ("\\(\alpha\\) or \\(\beta\\)") is a formula.
    - Examples for propositional calculus:
        - An example formula is \\( (\bbC ~ X ~ Y) \\).
        - An example of a string that is *not* a formula is \\( \bbC ~ X ~ Y \\) because it lacks the parentheses.
    - (Note to self)
        - We should be less formal.
        - We should teach the formation rules by example.
        - We should use standard symbols \\( \neg, \wedge, \vee \\).
        - Should we use \\( \& \\) instead of \\( \wedge \\)?
            - Do \\( \wedge \\) and \\( \vee \\) confuse newcomers?
- Now that we are familiar with the syntax of propositional calculus,
we can move on to a *predicate calculus*.
    - To the logical symbols, we add two *quantifier* symbols:
        - \\( \forall \\) ("for all", universal quantifier),
        - \\( \exists \\) ("there exists", existential quantifier).
    - Every capital letter in italic font is called a *relation symbol*.
    Every relation symbol has an *arity* that is a natural number;
    the arity is the number of parameters taken by the relation symbol.
    If the arity is zero, the relation symbol is also called a *constant symbol*.
    - The non-logical symbols of a predicate calculus is a set of relation symbols;
    each relation symbol looks like \\( A^n \\) where \\( n \\) is the symbol's *arity*;
    it is the number of arguments.
        - For example, we can define a predicate calculus whose set of relation symbols is <span>\( \{ E^2 \} \)</span>.
            - An example formula is then <span>\( E(x,y) \)</span>.
            - An example sentence is then <span>\( \forall x ~ E(x,x) \)</span>.
    - We call a predicate calculus has order one iff the quantifiers can only take constant symbols.
    - To the formation rules, we add:
        1. if \\( \rho^n \\) is a relation symbol of arity \\( n \\),
        and \\( \alpha_1, \ldots, \alpha_n \\) are variables, then \\( (\rho^n \alpha_1 \ldots \alpha_n) \\) is a formula;
        1. if \\( \rho^0 \\) is a relation symbol of arity zero, then \\( \rho^0 \\) is a formula;
        1. if \\( \alpha \\) is a formula and \\( \beta \\) is a formula, then <span>\( (\bbC \alpha \beta) \)</span> is a formula.
        1. if \\( v \\) is a variable, \\( Q \\) is a quantifier, and \\( F \\) is a formula, then \\( (Q v ~ F) \\) is a formula.
    - A *sentence* is something that can be given a truth value;
    in propositional calculus, it is a formula;
    in first-order predicate calculus, it is a formula with no free variables.
    An example of a first-order predicate calculus sentence is \\( \forall x : E(x,x) \\).
    An example of a first-order predicate calculus formula that is not a sentence is \\( E(x,x) \\).
    - An *interpretation* of a language is a function
    that takes a sentence of that language and gives a *truth value*.
    For example, if we have a graph,
    then we may map every term \\( x \\) to a vertex \\( I(x) \\) of the graph,
    and we may map the formula <span>\( R(x,y) \)</span>
    <span>\( I(R(x,y)) \)</span>, that is whether \\( (I(x), I(y)) \\) is an edge of the graph.

<div>\begin{align*}
I(R(x,y)) = E(I(x),I(y))
\end{align*}</div>

- How many truth values are there?
    - It depends on the logic.
    - In classical logic,
    there are *two* truth values: *false* and *true*.
    - In SQL (a language used to interact with relational databases),
    there are *three* truth values: *false*, *null*, and *true*.
    - In the IEEE 1164 standard, part of VHDL (a language for describing electronic circuits),
    there are *nine* truth values.
    - In fuzzy logic, there are as many truth values as there are real numbers in \\( [0,1] \\).
    - [WP:Four-valued logic](https://en.wikipedia.org/wiki/Four-valued_logic)
    - [WP:IEEE 1164](https://en.wikipedia.org/wiki/IEEE_1164)
- A *metalanguage* is a language that describes a language.
    - We used English as a metalanguage to describe propositional calculus.

Now we formalize.
Let \\( L \\) be a language,
let \\( F \\) be the set of formulas of \\( L \\),
and let \\( T \\) be the set of truth values of this interpretation.
Let <span>\( T = (\{0,1\},\neg,\wedge,\vee) \)</span> be a *Boolean algebra*.
(???)

An *interpretation* of a language \\( L \\)
is a function \\( I : F \to T \\).
This function must satisfy
\\( I((\bbN x)) = \neg I(x) \\),
and \\( I((\bbC x y)) = I(x) \wedge I(y) \\),
and \\( I((\bbD x y)) = I(x) \vee I(y) \\).

For example, <span>\( \{ x, y \} \)</span> is a model of <span>\( x \)</span>.

For example, <span>\( \{ x, y, (\bbC x y) \} \models (\bbC x y) \).

For example, <span>\( \{ (\bbC x y) \} \)</span> is *not* an interpretation of \\( L \\),
because if <span>\( (\bbC x y) \)</span> is in \\(I\\),
then both \\(x\\) and \\(y\\) must also be in \\(I\\).

We say that \\( I \\) *models* \\( p \\) or \\( I \\) is a *model* of \\( P \\),
written \\( I \models p \\),
iff \\( I(p) \\) is true.
We say that \\( p \\) is *satisfiable* iff \\( p \\) *has a model*, that is, iff there exists \\( I \\) such that \\( I \models p \\).
The symbol \\( \models \\) is called a "double turnstile".
Why do we bother inventing another notation (\\( \models \\)) for a notation that already exists (\\( \in \\))?

For example, we may choose to map \\( x \\) to "true" and everything else to "false".
We write \\( \\).

- Extensions of first-order logic:
    - modal logic

Given the formation rules of a language, we can
(1) *generate* all formulas
(2) *decide* whether a given string is a formula.

- The logics form a hierarchy.
    - propositional logic
    - first-order logic
    - second-order logic

This first-order language can describe itself,
where \\( L(\alpha) \\) is true iff \\( \alpha \\) is a string that is a Latin small letter alone:
<span>\begin{align*}
\forall \alpha : L(\alpha) &\implies W(\alpha)
\\ \forall \alpha : \forall \beta : W(\alpha) &\implies W((N \alpha))
\\ \forall \alpha : \forall \beta : W(\alpha) \wedge W(\beta) &\implies W((C \alpha \beta))
\\ \forall \alpha : \forall \beta : W(\alpha) \wedge W(\beta) &\implies W((D \alpha \beta))
\end{align*}</span>
and that can also be written using sequent calculus notation
with implicit universal quantification over free variables:
<span>\begin{align*}
L(\alpha) &\vdash W(\alpha)
\\ W(\alpha) &\vdash W((N \alpha))
\\ W(\alpha), W(\beta) &\vdash W((C \alpha \beta))
\\ W(\alpha), W(\beta) &\vdash W((D \alpha \beta))
\end{align*}</span>

- ["Propositional logic: models and proofs"](http://www3.cs.stonybrook.edu/~cram/cse505/Fall16/Lectures/proplogic.pdf),
by C. R. Ramakrishnan, 2016
- [Rough summary of first order logic](https://en.wikibooks.org/wiki/Finite_Model_Theory/Preliminaries), Finite Model Theory wikibook
- Model theory
    - Finite model theory
        - [Elements of finite model theory](http://homepages.inf.ed.ac.uk/libkin/fmt/fmt.pdf) by Leonid Libkins
        - [WP:Finite model theory](https://en.wikipedia.org/wiki/Finite_model_theory)
    - [Short course on model theory](http://mathstat.helsinki.fi/logic/people/jouko.vaananen/shortcourse.pdf)
    - [WP:Signature](https://en.wikipedia.org/wiki/Signature_(logic))
    - [WP:Structure](https://en.wikipedia.org/wiki/Structure_(mathematical_logic))
    - [WP:Löwenheim–Skolem theorem](https://en.wikipedia.org/wiki/Löwenheim–Skolem_theorem)
    - [WP:Model theory](https://en.wikipedia.org/wiki/Model_theory)
    - [WP:Double turnstile](https://en.wikipedia.org/wiki/Double_turnstile)
        - https://math.stackexchange.com/questions/469/what-is-the-meaning-of-the-double-turnstile-symbol-models
    - [WP:Logic of graphs](https://en.wikipedia.org/wiki/Logic_of_graphs)
- Formal logic
    - [Introduction to formal logic](http://web.uvic.ca/~tiberius/logic/formal.pdf), philosophy
    - [WP:Well-formed formula](https://en.wikipedia.org/wiki/Well-formed_formula)
    - [WP:Formal system](https://en.wikipedia.org/wiki/Formal_system)
    - [WP:Formal language](https://en.wikipedia.org/wiki/Formal_language)
    - [WP:Gödel's completeness theorem](https://en.wikipedia.org/wiki/G%C3%B6del%27s_completeness_theorem)
    - [WP:Gödel's incompleteness theorems](https://en.wikipedia.org/wiki/G%C3%B6del%27s_incompleteness_theorems)
    - [WP:Proof theory](https://en.wikipedia.org/wiki/Proof_theory)
    - [WP:Sequent calculus](https://en.wikipedia.org/wiki/Sequent_calculus)
        - Turnstile symbol
- Ungrouped
    - [WP:Mathematical logic](https://en.wikipedia.org/wiki/Mathematical_logic)
    - [Logic and probability](https://plato.stanford.edu/entries/logic-probability/), Stanford Encyclopedia of Philosophy
    - [WP:Semantic theory of truth](https://en.wikipedia.org/wiki/Semantic_theory_of_truth)
    - [WP:Tarski's definition of truth: T-schema](https://en.wikipedia.org/wiki/T-schema)
    - [Computational Logic Course Material](http://cliplab.org/logalg/)
    - [WP:List of first-order theories](https://en.wikipedia.org/wiki/List_of_first-order_theories)
    - [WP:Realizability](https://en.wikipedia.org/wiki/Realizability)
    - [WP:Universal quantification, universal closure](https://en.wikipedia.org/wiki/Universal_quantification#Universal_closure)
    - Math fonts
        - [WP:Fraktur](https://en.wikipedia.org/wiki/Fraktur)
        - [WP:Blackboard bold](https://en.wikipedia.org/wiki/Blackboard_bold)

## Mess

In propositional logic, it takes at most \\(O(n)\\) steps to determine whether a string of length \\(n\\) is a formula.

A _variable_ is any of the 26 Latin small letters from \\(a\\) to \\(z\\).

In *propositional logic*, we represent a sentence with a letter.
For example, we can use the letter \\( p \\) to represent "John is lecturing"
and the letter \\( q \\) to represent "John is awake".
From those two sentences, we can construct another sentence \\( p \to q \\)
that represents "If John is lecturing, then John is awake".

A *logical system* (a "logic") is a *formal system* and an *interpretation*.
A formal system has a *syntax* and some *inference rules*.
The syntax tells us how to form *formulas*.
A syntax is a set of rules that determine which strings are formulas.
The inference rules tell us how we can rewrite a formula to another formula
while preserving the truth of the formula.
A formula has no inherent meaning,
but we can give meaning to it by defining an *interpretation*.
An *interpretation* maps a formal system to a *model*.

A *sentence* is a formula with no free variables.
A *formula* is a statement whose truth can be determined, that is either true or false.
For example, "John is lecturing" and \\( 1+1 = 2 \\) are statements.
Later we will see that there are other ways of defining "truth".

A *signature* \\( \sigma \\) is a triple of
a set of relation symbols,
a set of function symbols,
and an arity function.
A *structure* \\( \struc{A} \\) is a triple \\( (A,\sigma,I) \\) where \\(A\\) is a domain,
\\(\sigma\\) is a signature, and \\( I \\) is an *interpretation function*.

A formal argument is an argument that is made by blindly following the rules,
by mechanically following the rules to manipulate symbols,
without any meaning, without any guesswork.
This allows computers to help us.

- Readings?
    - [Model theory](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.456.6021&rep=rep1&type=pdf), 1970 article, Howard Jerome Keisler.
    It is accessible and it still gives a good and relevant introduction in 2017.
    - [Logic terms and concepts](https://www.ics.uci.edu/~alspaugh/cls/shr/logicConcepts.html)
        - Must distinguish logic and *meta-logic*.
    - ???
        - Model theory, 1989 book, Chang and Keisler
    - Barwise 1989 handbook of mathematical logic 8th impression 1999
        - Understand logic terms by formalizing group theory
    - [A short course on finite model theory](http://www.math.helsinki.fi/logic/people/jouko.vaananen/shortcourse.pdf) by Jouko Väänänen

Here we will review mathematical logic and model theory.

Mathematical logic allows us to represent English in symbols without ambiguity.
Logic also works with other human languages, not only English.

A key idea in logic is the separation between *form* and *meaning*.
The validity of an argument depends only on its form?

Every argument of this form (modus ponens) is valid:
<div>\begin{align*}
p, ~ p \to q \vdash q
\end{align*}</div>

Abductive reasoning (physics?):

<div>\begin{align*}
p, ~ q \vdash_? p \to q
\end{align*}</div>

In classical deductive logic, there is only one way to reach valid conclusion:
by valid premises and valid argument.
There are three ways to arrive at an invalid conclusion:
    - by invalid premises but valid argument,
    - by valid premises but invalid argument,
    - by invalid premises and invalid argument.

In logic, an interpretation assigns truth value to well-formed formulas.

A system is *sound* iff every provable sentence is true: <span>\(A \vdash B \implies A \models B\)</span>.

A system is *complete* iff every true sentence is provable <span>\(A \models B \implies A \vdash B\)</span>.

https://en.wikibooks.org/wiki/Formal_Logic/Predicate_Logic/Satisfaction

*Intended interpretation* is synonym for *standard model*.

We use [s-expressions](https://en.wikipedia.org/wiki/S-expression) to simplify parsing.

Greek letters are part of the _metalanguage_ (the language describing the _object language_).

Example term: \\((A (A a b) (N c))\\).

### Interpretation

An _interpretation_ is a function that takes a term.

Example interpretation:

<div>\begin{align*}
I(a) &= 1
\\
I(b) &= 1
\\
I(c) &= 0
\\
I((A \alpha \beta)) &= I(\alpha) \wedge I(\beta) = \min(I(\alpha),I(\beta))
\\
I((N \alpha)) &= \neg I(\alpha) = 1 - I(\alpha)
\end{align*}</div>

where \\(0\\) means "false" and \\(1\\) means "true".

Applying the rules recursively to the example term gives

<div>\begin{align*}
I((A(Aab)(Nc))) &= I((Aab)) \wedge I((Nc))
\\ &= (I(a) \wedge I(b)) \wedge I((Nc))
\\ &= (1 \wedge I(b)) \wedge I((Nc))
\\ &= (1 \wedge 1) \wedge I((Nc))
\\ &= 1 \wedge I((Nc))
\\ &= 1 \wedge \neg I(c)
\\ &= 1 \wedge \neg 0
\\ &= 1 \wedge 1
\\ &= 1.
\end{align*}</div>

## Probability logic

- First-order probability logic (FOPL) shares the same syntax as first-order logic (FOL), but different interpretation:
    - FOL interpretation maps FOL wff to truth value <span>\( \{0,1\} \)</span>.
    - FOPL interpretation maps FOL wff to probability <span>\( [0,1] \)</span>.

- Boolean algebra is a special case of fuzzy logic?
    - Replace \\(\\{0,1\\}\\) (the set of Boolean values) with \\([0,1]\\) (the set of real numbers in the unit line).
    - [WP:Boolean algebra](https://en.wikipedia.org/wiki/Boolean_algebra)
    - [WP:Fuzzy logic](https://en.wikipedia.org/wiki/Fuzzy_logic)

<div>\begin{align*}
t(p \wedge q) &= \min(t(p), t(q))
\\ t(p \vee q) &= \max(t(p), t(q))
\\ t(\neg p) &= 1 - t(p)
\end{align*}</div>

- Fuzzy logic is a special case of probability space?
    - [WP:probability space](https://en.wikipedia.org/wiki/Probability_space).
    - What should \\(t(p \to q)\\) be?

Classical logic:

<div>\begin{align*}
t(p \to q) &= t(\neg p \vee q) = \max(1 - t(p), t(q))
\end{align*}</div>

Bayesian:

<div>\begin{align*}
t(p \to q) &= t(q|p) = \frac{t(q \wedge p)}{t(p)}
\end{align*}</div>

Induction:

<div>\begin{align*}
\exists a (p(a) \wedge q(a)) \vdash_i \forall x (p(x) \wedge q(x))
\\
T(p) \subseteq T(q) \vdash_i \forall x (p(x) \implies q(x))
\\
T(p) = \{ x ~|~ p(x) \}
\end{align*}</div>
