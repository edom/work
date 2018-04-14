---
title: Intelligence
permalink: /intelligence.html
date: 2017-06-22 03:57:00 +0700
mathjax: true
---

## Research questions

- Research questions:
    - What is intelligence? How should it be defined?
    - [What are some of the best books on AI/ML?](https://www.reddit.com/r/artificial/comments/8begcv/what_are_some_of_the_best_books_on_artificial/)
    - [Math PhD. Want to learn more about AI. What to read?](https://www.reddit.com/r/artificial/comments/8bzrmd/math_phd_want_to_learn_more_about_ai_what_to_read/)
    - How do we make an AI?
    - How do we create a seed AI?
    - Hume's question: How do we justify generalization? Why does generalization work?
        - Does Wolpert answer that in "no free lunch theorem"?
- History questions:
    - Why was Raymond J. Solomonoff \cite{SolAlpProb2011, GacsVitanyiSolomonoff} interested in predicting sequences of bits?

### Intelligence as an ordering: How do we decide which of two systems is more intelligent?

Intelligence can be thought as an *ordering* (transitive antisymmetric relation) of systems.
We agree that a dog is more intelligent than a rock.

*How do we decide which of two systems is more intelligent?*

Suppose that we can measure whether entity \\( A \\) is *better* than entity \\( B \\) at task \\( T \\).
Then we can say that \\( A \\) is *more intelligent* than \\( B \\) in the task set \\( S \\)
iff \\( A \\) is better than \\( B \\) at every task in \\( S \\).
Note the qualifier "in the task set \\( S \\)".
Intelligence depends on how it's measured.

[Edwin G. Boring in 1923](https://brocku.ca/MeadProject/sup/Boring_1923.html)
proposed that we start out by defining intelligence as what intelligence tests measure
"until further scientific observation allows us to extend the definition".
That definition makes sense mathematically.

## What is intelligence?

The word "intelligent" comes from a Latin word that means "to choose between"
([Dictionary.com](http://www.dictionary.com/browse/intelligent)).

[Shour2018](https://www.researchgate.net/publication/323203054_Defining_intelligence):
"Defining intelligence as a rate of problem solving and using the concept
11 of network entropy enable measurement, comparison and calculation of
12 collective and individual intelligence and of computational capacity."

"Intelligence measures an agent's ability to achieve goals in a wide range of environments."
[Legg2006][Legg2008]

Tononi integrated information theory.
[Wikipedia](https://en.wikipedia.org/wiki/Integrated_information_theory).

Schmidhuber, Hutter, and team have used Solomonoff algorithmic probability
and Kolmogorov complexity to define a theoretically optimal predictor they call AIXI.
J\"urgen Schmidhuber.
[Schmidhuber article](http://www.idsia.ch/~juergen/newai/newai.html).
[Prashant slides](http://www.cs.uic.edu/~piotr/cs594/Prashant-UniversalAI.pdf).
Prashant's slides define "universal" and "optimal".

Marcus Hutter approached intelligence from \emph{algorithmic} complexity theory (Solomonoff induction)
\cite{DefineMachIntel}.
Warren D. Smith approached intelligence from \emph{computational} complexity theory
(NP-completeness)
\cite{WdsIntel, WdsIntelSlide}

Legg and Hutter has collected definitions of intelligence in [Legg2007Collection].
We limit our concern to mathematical definitions.
