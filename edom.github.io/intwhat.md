---
title: Defining intelligence
permalink: /intwhat.html
date: 2017-06-22 03:57:00 +0700
mathjax: true
---

## Intelligence is an ordering (2018-04-26)

This idea goes back at least to 2005 in [Hutter2005Book] (p. 2).

Intelligence is an *ordering* of systems.

An order is a transitive antisymmetric relation.

[Edwin Boring in 1923](https://brocku.ca/MeadProject/sup/Boring_1923.html)
proposed that we start out by defining intelligence as what intelligence tests measure
"until further scientific observation allows us to extend the definition".
That definition makes sense mathematically.

*Intelligence depends on its measurement*. Absolute intelligence doesn't exist.

- The *behavior* of a system is whatever it exhibits that can be observed from outside.

### How do we decide which system is more intelligent?

Let \\( A \\) be a system.

Let \\( B \\) be a system.

Let \\( T \\) be a task.

Let \\( S \\) be a set of tasks.

Let \\( T(A) \\) denote how well system \\( A \\) does task \\( T \\).
This is a number.
Higher is better.
We can invent any measurement.
Our definition of "intelligence" is only as good as this measurement.

We say "\\( A \\) is *\\( T \\)-better* than \\( B \\)" iff \\( T(A) > T(B) \\).

We say "\\( A \\) *\\( S \\)-dominates* \\( B \\)" iff \\( T(A) > T(B) \\) for every task \\( T \in S \\).

We define "to be more \\( S \\)-intelligent than" to mean "to \\( S \\)-dominate".

The \\( S \\)-domination relation forms a partial order of all systems.

That is how.

#### Example

Which is more intelligent, a dog or a rock?

That depends on the task set \\( S \\).

It's the rock if <span>\( S = \{ \text{sit still} \} \)</span>.

It's the dog if <span>\( S = \{ \text{move around} \} \)</span>.

## Intelligence is function optimization (2018-04-27)

Let \\( g \\) be a goal function.

A system's \\( g \\)-intelligence is how well it optimizes \\( g \\).

What is "how well"?

Optimization (extremization) is either minimization or maximization.

## What is a mathematical theory of intelligence?

- In 2007, on page 12, in the paper [Universal intelligence: a definition of machine intelligence](https://arxiv.org/pdf/0712.3329.pdf),
Shane Legg and Marcus Hutter wrote,
"Intelligence measures an agent’s ability to achieve goals in a wide range of environments,"
and then they formalized them.
Here I try another formalization.
    - Let \\(E\\) be a set of *environments*.
    - Let \\(G : E \to \Real\\) be a *goal function*.
    The value of \\(G(e)\\) measures how well the agent performs in environment \\(e\\).
    - The *intelligence* of the agent *with respect to \\(G\\) across \\(E\\)* is \\( \int_E G \\).
    - A *performance* consists of an agent and an environment.
    - Assumption: The agent cannot modify \\(G\\).
    - Behavior is a function taking an environment and outputing something.
    - Intelligence is *relative* to \\(G\\) and \\(E\\): *goal* and *environment*.
    - If we see longevity as intelligence test,
then an illiterate farmer who lives to 80
is more intelligent than a scientist who dies at 20,
but a rock that has been there for 100 years would even be more intelligent than the farmer.
    - If we see money as intelligence test,
then a corrupt politician who steals billions of dollars without getting caught
is more intelligent than a honest farmer who only has tens of thousands of dollars.
- Gaming the system is a sign of intelligence.
It is hard to design a goal function that gives the desired outcome without undesired side effects.
- IQ tests are intelligence measures with small environment set.
- Lifespan may be an intelligence measure with huge environment set.
- A human can optimize *several* goal functions across the same environment set.
A human may be asked to clean a floor, to write a report, to run a company, to cook food,
and to find the quickest route between home and office,
and optimize them all.
- Some goal functions for humans are (but perhaps not limited to):
    - Maximize happiness
    - Minimize pain
    - Optimize the level of a chemical in the brain
    - Optimize the time integral of such chemical
    - Maximize the chance of survival
- but I don't know the root goal function
that explains all those behaviors.
- Where does the word "intelligence" come from? What is its etymology?
    - The word "intelligent" comes from a Latin word that means "to choose between"
    ([Dictionary.com](http://www.dictionary.com/browse/intelligent)).
- What are some mathematical definitions of intelligence?
    - "Intelligence measures an agent's ability to achieve goals in a wide range of environments."
    [Legg2006][Legg2008]
    - [Shour2018](https://www.researchgate.net/publication/323203054_Defining_intelligence):
    "Defining intelligence as a rate of problem solving and using the concept
    of network entropy enable measurement, comparison and calculation of
    collective and individual intelligence and of computational capacity."
    - Tononi integrated information theory.
    [Wikipedia](https://en.wikipedia.org/wiki/Integrated_information_theory).
    - Schmidhuber, Hutter, and team have used Solomonoff algorithmic probability
    and Kolmogorov complexity to define a theoretically optimal predictor they call AIXI.
        - J\"urgen Schmidhuber. [Schmidhuber article](http://www.idsia.ch/~juergen/newai/newai.html).
        - [Prashant's slides](http://www.cs.uic.edu/~piotr/cs594/Prashant-UniversalAI.pdf).
            These define "universal" and "optimal".
    - Marcus Hutter approached intelligence from \emph{algorithmic} complexity theory (Solomonoff induction)
    \cite{DefineMachIntel}.
    - Warren D. Smith approached intelligence from \emph{computational} complexity theory
    (NP-completeness)
    \cite{WdsIntel, WdsIntelSlide}
- What are other definitions of intelligence?
    - Legg and Hutter has collected definitions of intelligence in [Legg2007Collection].

## What is learning?

- There are so many ML algorithms.
What's the common thing?
- Should I read these?
    - [How To Become A Machine Learning Engineer: Learning Path](https://medium.com/machine-learning-world/learning-path-for-machine-learning-engineer-a7d5dc9de4a4)
    - https://dzone.com/guides/artificial-intelligence-machine-learning-and-predi
- What is the relationship between ML and statistical modeling?
- How do we categorize ML algorithms?
    - Online vs offline
        - [Wikipedia: Online machine learning](https://en.wikipedia.org/wiki/Online_machine_learning)
    - Discrete-time model vs continuous-time model
        - LTI (linear time-invariant) systems
    - Assemble answers from these sources:
        - [Wikipedia: Machine learning, approaches](https://en.wikipedia.org/wiki/Machine_learning#Approaches)
        - [Wikipedia: Outline of machine learning, algorithms](https://en.wikipedia.org/wiki/Outline_of_machine_learning#Machine_learning_algorithms)
        - [Wikipedia: Outline of machine learning, methods](https://en.wikipedia.org/wiki/Outline_of_machine_learning#Machine_learning_methods)
        - [A tour of machine learning algorithms](https://machinelearningmastery.com/a-tour-of-machine-learning-algorithms/)
        - [Types of machine learning algorithms you should know](https://towardsdatascience.com/types-of-machine-learning-algorithms-you-should-know-953a08248861)
        - [Stats SE 214381: mathematical definition of classifier](https://stats.stackexchange.com/questions/214381/what-exactly-is-the-mathematical-definition-of-a-classifier-classification-alg)
        - [Common machine learning algorithms](https://www.analyticsvidhya.com/blog/2017/09/common-machine-learning-algorithms/)
- What is a neural network?
    - A *neuron* is a function in \\( \Real^\infty \to \Real \\).
    - A *neural network* layer is a function in \\( \Real^\infty \to \Real^\infty \\).
    - Why do neural networks work?
        - [Wikipedia: Universal approximation theorem](https://en.wikipedia.org/wiki/Universal_approximation_theorem)
- Statistical learning
- What is backpropagation, from functional analysis point of view?
- Who are AI/ML researchers and what are their focuses?
    - Does Geoffrey Hinton specialize in image recognition?
- What is the relationship between intelligence and compression?
- Consider endofunctions of infinite-dimensional real tuple space.
That is, consider \\( f, g : \Real^\infty \to \Real^\infty \\).
    - What is the distance between them?
- Reductionistically, a brain can be thought as a function in \\( \Real \to \Real^\infty \to \Real^\infty \\).
    - The first parameter is time.
    - The second parameter is the sensor signals.
    - The output of the function is the actuator signals.
    - Can we model a brain by such
    [functional differential equation](https://en.wikipedia.org/wiki/Functional_differential_equation)
    involving [functional derivative](https://en.wikipedia.org/wiki/Functional_derivative)s?
    - \\( \norm{f(t+h,x) - f(t,x)} = h \cdot g(t,x) \\)
    - \\( \norm{f(t+h) - f(t)} = h \cdot g(t) \\)
    - It seems wrong. Abandon this path. See below.
- We model the input as a function \\( x : \Real \to \Real^n \\).
- We model the output as a function \\( y : \Real \to \Real^n \\).
    - \\( \norm{y(t+h) - y(t)} = h \cdot g(t) \\)
    - \\( y(t+h) - y(t) = h \cdot (dy)(t) \\)
    - \\( \norm{(dy)(t)} = g(t) \\)
        - There are infinitely many \\( dy \\) that satisfies that. Which one should we choose?
    - If \\( y : \Real \to \Real^n \\) then \\( dy : \Real \to \Real^n \\).
- A classifier is a function in \\( \Real^\infty \to \Real \\).
- A control system snapshot is a function in \\( \Real^\infty \to \Real^\infty \\).
- A control system is a function in \\( \Real \to \Real^\infty \to \Real^\infty \\).
- How does \\( F \\) have memory if \\( F(t) = \int_0^t f(x) ~ dx \\)?

Why has AI mastered chess, but not real life?
Because chess search space is much smaller than real-life search space.

## What is AI?

- In the 1950s, AI was whatever McCarthy et al. were doing.
    - "McCarthy coined the term 'artificial intelligence' in 1955, and organized the famous Dartmouth Conference in Summer 1956.
    This conference started AI as a field."
    ([WP: John McCarthy (computer scientist)](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)))
    - [WP: Dartmouth workshop](https://en.wikipedia.org/wiki/Dartmouth_workshop)
    - [Ray Solomonoff's Dartmouth archives](http://raysolomonoff.com/dartmouth/)
- What are AI approaches? How are we trying to make an AI?
    - Pedro Domingos categorizes AI approaches into five *tribes*:
        - symbolists (symbolic logic)
        - connectionists (neural networks)
        - evolutionaries (genetic algorithms)
        - bayesians (statistical learning, probabilistic inference)
        - analogizers (what is this?)
- How do we measure intelligence? How do we measure the performance of a learning algorithm?
    - [Wikipedia: Computational learning theory](https://en.wikipedia.org/wiki/Computational_learning_theory)
        - What is the goal of computational learning theory?
            - "Give a rigorous, computationally detailed and plausible account of how learning can be done." [Angluin1992]
        - "a subfield of Artificial Intelligence devoted to studying the design and analysis of machine learning algorithms"
        - What is a mathematical theory of learning?
            - What is learning?
                - 2018-04-19: "To learn something" is to get better at it.
                Usually learning uses experience.
                    - What is the formal definition of "get better"?
                        - Let there be a system.
                        Pick a task.
                        Pick a time interval.
                        Test the system several times throughout the time interval.
                        Let the test results be the sequence \\( X = x_1, x_2, \ldots, x_n \\).
                        We say that the system is *learning* the task in the time interval
                        iff \\( x_1 < x_2 < \ldots < x_n \\)
                        (that is iff \\( X \\) is a monotonically increasing sequence).
                        - How do we formalize "get better" and "experience"?
                            - "Get better" can be modeled by *monotonically increasing score*
                            - "Experience" can be modeled by a sequence
                    - Is experience (past data) necessary for learning?
                    Are mistakes necessary for learning?
                - Supervised learning is extrapolating a function from finite samples.
                Usually, the function is high-dimensional, and the samples are few.
                - It is simple to measure learning success in perfect information games such as chess.
                Chess also doesn't require any sensors and motors.
