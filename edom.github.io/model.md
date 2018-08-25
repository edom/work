---
title: Modeling
date: 2018-05-24 03:02 +0700
permalink: /model.html
mathjax: yes
---

## Abstract
{:.no_toc}

We want to replace this abstract with the answers of these questions:

- What is a model?
What does "X is a model of Y" mean?
- How does X model Y?
- How do we measure how good a model is?
- How do we formalize all that?
- How does it relate to model theory?

## Contents
{:.no_toc}

* TOC
{:toc}

## What is a model?

### Tracing the roots of "model"

[Wiktionary:model](https://en.wiktionary.org/wiki/model) says that "model" comes from Latin "modellus" that is diminutive of "modulus" that is diminutive of "modus" that means "measure".
Thus "modellus" should mean "a small small measure".

What is the history of the word "model"?
How did people use it in the past?
Use [Google Ngram](https://books.google.com/ngrams) to find how words were used in the past, as far back as the 18th century.

### Inferring the meaning of "model" from example sentences

Here we try to infer the meanings of "model" from these sources:

- The definitions of "model" in these dictionaries:
[Princeton WordNet 3.1](http://wordnetweb.princeton.edu/perl/webwn?s=model&sub=Search+WordNet&o2=&o0=1&o8=1&o1=1&o7=&o5=&o9=&o6=&o3=&o4=&h=),
[English Oxford Dictionaries](https://en.oxforddictionaries.com/definition/model),
[The Free Dictionary](https://www.thefreedictionary.com/model),
[Wiktionary](https://en.wiktionary.org/wiki/model#Etymology).
Also, the definition of "model oneself after" in [Merriam-Webster](https://www.merriam-webster.com/dictionary/model%20oneself%20after).
- Other people's opinions:
    - [systems-thinking.org](http://www.systems-thinking.org/simulation/model.htm): A model is a simplification of reality.
    - 2010 Emily Griffiths article "What is a model?" [archived pdf](https://web.archive.org/web/20120312220527/http://www.emily-griffiths.postgrad.shef.ac.uk/models.pdf)
    - [What is a model? - Talk to Books](https://books.google.com/talktobooks/query?q=What%20is%20a%20model%3F)
- Encyclopedia articles
    - Wikipedia article [Model](https://en.wikipedia.org/wiki/Model), and all pages linked from there, such as
    [Architectural model](https://en.wikipedia.org/wiki/Architectural_model),
    [Physical model](https://en.wikipedia.org/wiki/Physical_model),
    Also, by text search: [Role model](https://en.wikipedia.org/wiki/Role_model).
    - [Encyclopaedia Britannica search result for "model"](https://www.britannica.com/search?query=model)
    - [encyclopedia.com search result for "model"](https://www.encyclopedia.com/search?keys=model)

The word "model" have many meanings.
Here are some example sentences.
We are only interested in Sense 1.
But can we show that the other senses are just special cases of Sense 1?

- Sense 1: concept
    - Sense 1a: concretion for transfer learning
        - "A *model organism* is [...] extensively studied [...]
        with the expectation that discoveries made in the model organism will provide insight into [...] other organisms."
        ([WP:Model organism](https://en.wikipedia.org/wiki/Model_organism))
        This suggests that a model is about *transfer learning*:
        a knowledge about the model maps to a knowledge about the modeled,
        if the model captures the required aspects.
    - Sense 1b: abstraction, picking aspects, ignoring irrelevant details
        - A Newton equation system *models* a real physical system.
        - A Newton equation system *is a model of* a real physical system.
        - Quadratic equations *model* parabolic motions.
        - The Lotka-Volterra equations can *model* the populations of lions and deers in a savanna.
    - Note: It is confusing that "model" can mean both concretion and abstraction, which are opposites.
    - Sense 1c: a resemblance that takes less effort to make than the real thing does
        - This *model* railroad is mostly made of plastic.
        - This miniature *models* the 20-story building.
        - This miniature *is a model of* the 20-story building.
        - A stick figure is a *model* of a person/animal/being.
    - Sense 1c: intended resemblance
        - Some cartoon characters *are modeled after* real people.
        - The Abraham Lincoln statue *was modeled after* Abraham Lincoln.
        (It means that the statue was intended to look like him.)
- Sense 2: person, animal, being
    - Sense 2a: fashion model, photo model, actor
        - The *model* sits on the chair so that the painters can begin painting.
        - The *model* wearing a fancy dress walks on the catwalk.
        - The *model* is posing for the camera.
        - His dog is *modeling* for a dog food advertisement.
    - Sense 2b: what one strives to imitate
        - He is a *role model*.
        - He is a *model* husband. He and his wife are much happier after their marriage.
- Sense 3: type, class, kind, variant, product line
    - Ford *Model* T and Tesla *Model* S are cars.
    - This Honda car comes in two *models*: an automatic transmission *model* and a manual transmission *model*.
    - Which *model* of this Toyota car do you want to buy: the gasoline engine *model* or the diesel engine *model*?

A model is ... resemblance, replica, downscaling, simplification?
We say "X models Y" iff X resembles Y, iff X behaves like Y, iff X is a simplification of Y, iff X and Y have something in common but X is simpler than Y?
A model of X is a simplified representation of X.
A model is a simplified description of reality?
A description of reality is not reality.
If reality and theory disagree, then reality wins and theory must change.

To model something is to choose which aspects to care about.
All other aspects are ignored.

Obvious implication: A model is not the real thing.

A model is analogization.

"To model X" is to deliberately lose irrelevant information about X.

"X models Y" means "Y is X under some simplifying assumptions".

"X models Y" means "Y is X with some details lost".

The tuple (john, 30 years old) models the tuple (john, 30 years old, black hair, brown eyes).
The capturing function (the modeling schema) is \\( (a,b,c,d) \mapsto (a,b) \\).
Modeling is dimension reduction.
Modeling is projection.

Another example.
Statics.
Real physical systems in rest.
Pick a coordinate system.
Center of mass of car.
Change car color.
The model is the same.
The reality is different.

Some models model reality well.
Some reality is modeled well.
Some models are unrealistic.
Some reality are unmodelable.

Example: (john, 100000 years old).

Let X be the set of all people in company C.
Let Y be the set of all tuples (name, age).

(john, 1000 years old) is in Y, but f doesn't map it.

### Defining "model" generally

We say any of these to mean the same thing:

- "The set X models the set Y with capturing function f."
- "The function f models Y with X."

The meaning is "There are subsets \\( X' \subseteq X \\) and \\( Y' \subseteq Y \\)
such that \\( f : Y' \to X' \\) is surjective."

The "capturing function" defines the aspects of reality that we capture in our model.
The function describes how X model Y.

Corollary: Every set models itself.
This is the theoretically correct but practically useless 1:1 map.

Here are some examples of "X models Y" to show the generality of that definition:

In software engineering, we can map Y (an employee) to X (a row in the database), but then we lose some irrelevant information about Y, such as hair color, weight.

In physics, we can map Y (a real physical system) to X (a Newton equation system) by assuming certain things (coordinate systems, point masses, absolute time),
but we can't map X to Y because in reality there are no point masses and time is relative.
By "Newton equation system", we mean a system of equations whose every equation looks like \\( F_k = m_k \cdot a_k \\).

In geometry, we can map Y (a 3D vector) to X (a 2D vector).
Thus geometric projection is modeling.

In analysis, we can map Y (the exponential function) to X (a truncated Taylor series of the exponential function).
Thus approximation is modeling.
Related: [approx]({% link approx.md %}), [atrunc]({% link atrunc.md %}).

In real life, a writing is a model of its author.
What you think about X is your model of X.
Everyone models everyone they have ever encountered.
Our thoughts model reality.
Brain activity models reality.
Our thoughts of ourselves model ourselves.

Consider X = the set of all white cars and Y = the set of all cars.
Obviously X is a subset of Y.
X models Y.

## Modeling: How does X model Y?

Consider several ways we can model a person:

- as a stick figure
- as a photograph
- as a police sketch
- as a tuple (row in a relational database)
- as a "chemical" that may react with another person ("chemical")

Reading list:

- 1980 Hilary Putnam article "Models and Reality" [paywall](https://www.jstor.org/stable/2273415?seq=1#page_scan_tab_contents)
- 2004 Ronald N. Giere article "How Models Are Used to Represent Reality" [pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.433.860&rep=rep1&type=pdf)

## Measuring model quality: How well does X model Y?

- ["All models are wrong, but some are useful."](https://en.wikipedia.org/wiki/All_models_are_wrong), George Edward Pelham Box (1919--2013)
- In [The Relativity of Wrong](http://chem.tufts.edu/answersinscience/relativityofwrong.htm), Isaac Asimov writes
    - Asimov's relativity of wrong:
        - Both round earth theory and flat earth theory are wrong,
        - but believing that they are equally wrong is wronger than both of them combined.
- https://en.wikipedia.org/wiki/The_Relativity_of_Wrong
- https://en.wikipedia.org/wiki/Wronger_than_wrong
- 2003 article "Measuring the Quality of Data Models: An Empirical Evaluation of the Use of Quality Metrics in Practice" [pdf](https://pdfs.semanticscholar.org/0536/3b8ddecd695444dc9b341796a0dc73e606be.pdf)
- [Measuring the Quality of Models \| TDAN.com](http://tdan.com/measuring-the-quality-of-models/4877)

## Relating to model theory?

Our definition of "model" above includes the notion of "model" in model theory?

What is a "model" in model theory?
Here I try to paraphrase
the 2000 David Marker book "Introduction to Model Theory" [pdf](http://library.msri.org/books/Book39/files/marker.pdf)
and the 2000 Weiss--D'Mello book "Fundamentals of Model Theory" [pdf](http://www.math.toronto.edu/weiss/model_theory.pdf).
I may err.
My paraphrase:

- A *structure* of a formal language \\( L \\) is a pair \\( (A,I) \\), where \\( A \\) is a set called the *universe*,
and \\( I \\) is the structure's *interpretation function*.
Such structure must also satisfy these:
    - Every constant symbol \\( c \in L \\) maps to a universe element \\( I(c) \in A \\).
    - Every relation symbol \\( R \in L \\) maps to a relation \\( I(R) \subseteq A^n \\), with the same arity \\( n \\).
    - Every function symbol in \\( F \in L \\) maps to a function \\( I(F) : A^n \to A \\), with the same arity \\( n \\).
    Note that a function of arity \\( n \\) is a kind of relation with arity \\( n + 1 \\).
    See [WP:Arity](https://en.wikipedia.org/wiki/Arity).
- Structure \\( A \\) *models* sentence \\( \varphi \\), written \\( A \models \varphi \\), iff ... ?
- I think I forget something.
Read Chapter 0 of the book.
It has examples.

The codomain of the interpretation function \\( I : L \to J(A) \\) is defined as follows.

- Every element of \\( A \\) is also in \\( J(A) \\).
- Every function \\( F : A^n \to A \\) is also in \\( J(A) \\), for every \\( n \\) that makes sense.
- Every relation \\( R \subseteq A^n \\) is also in \\( J(A) \\), for every \\( n \\) that makes sense.
(This makes the previous bullet point redundant.)
- That's all.

In other words:
\\[
J(A) = A + 2^{A^0} + 2^{A^1} + \ldots + 2^{A^n} + \ldots
\\]

I think \\( J(A) \\) is related to a Herbrand universe, but how?

The structure \\( S = (A,I) \\) maps the language \\( L \\) to \\( J(A) \\).

...
