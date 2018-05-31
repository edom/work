---
title: Computation
mathjax: yes
date: 2017-06-29 22:40 +0700
permalink: /computation.html
---

- What is the theory of computation?
    - What is it concerned with?
    - What are its important results?
- Where should we begin?
    - Should we begin by defining a function, a machine,
    computation, complexity, or a problem class?
    - Suppose we have a function \(f : A \to B\).
    - Given an input function \(\alpha : A \to S\),
    an output function \(\beta : S \to B\),
    and a transition function \(m : S \to S\),
    the computation is \(f = \beta \circ m^\infty \circ \alpha\).
    - The space of all functions from \(A\) to \(B\): \(F~A~B\).
    - We know that a function is a kind of relation.
    A relation composes.
    \((S, \circ)\) where \(\circ\) is associative.
    There is identity relation.
    There is inverse relation.
    Therefore the relation space forms a group.
    The function space forms a semigroup.
- ontology?
    - computable
        - [WP:Computable function](https://en.m.wikipedia.org/wiki/Computable_function)
    - computation
    - problem
        - [WP:Computational problem](https://en.wikipedia.org/wiki/Computational_problem)
        - [WP:Decision problem](https://en.wikipedia.org/wiki/Decision_problem)
    - machine
        - A *machine* is a formal system.
        - An FA (*finite automaton*) is ...
        - An FSM (*finite-state machine*) is ...
        - A *Turing-machine* is an FSM with infinite memory.
            - [WP:Turing machine, formal definition](https://en.wikipedia.org/wiki/Turing_machine#Formal_definition)
            - Turing-completeness
                - [WP:Turing-completeness](https://en.wikipedia.org/wiki/Turing_completeness)
                - A formal system is *Turing-complete* iff it can simulate every TM (Turing machine).
                    - What does it mean to *simulate* a TM?
                - [StackExchange CS theory 36863 formal definition of Turing-completeness](https://cstheory.stackexchange.com/questions/36863/formal-definition-of-turing-completeness):
                    - Kaveh suggested:
                        - [Robin Gandy: Church's Thesis and Principles for Mechanisms](https://www.sciencedirect.com/science/article/pii/S0049237X08712576)
                        - Classical recursion theory volume 1
            - Partial Turing machine, total Turing machine
                - What is the significance of the theorem in [WP:Total Turing machine](https://en.wikipedia.org/wiki/Total_Turing_machine)?
                - What is the relationship between total function and total Turing machine?
    - program
    - algorithm
        - An *algorithm* is a finite description that can be executed by a machine.
    - computation graph
        - ? \\(M\\) *simulates* \\(N\\) iff a subgraph of \\(M\\) is isomorphic to \\(N\\).
            - \\(T\\) is the type of terms.
            - \\(M\\) is the semantic function.
        - Shortcut
            - The *shortcut* of \\(A\\) is \\( B \\) where
            \\[
            B(x,y) = \text{\\(x\\) is initial, \\(y\\) is terminal, and \\(x\\) reaches \\(y\\).}
            \\]
                - \\(A\\) should be acyclic.
            - Alternative names for shortcut:
            [WP:Disintermediation](https://en.wikipedia.org/wiki/Disintermediation),
            immediation,
            curtailment,
            shortening,
            abridgement.
            - Shortcutting is idempotent:
            the shortcut of the shortcut is the shortcut itself.
- Do not confuse a _problem_ and an _algorithm_ that solves that problem.
    - Example:
    Consider \\(p(x)\\) that
    wastes \\(2^{|x|}\\) steps,
    and then returns the leftmost bit of \\(x\\).
    Thus \\(p \in \ExpTime\\),
    but \\(\Search(p) \in \Time(O(1))\\),
    because every string that begins with \\(1\\) satisfies \\(p\\),
    and we can just hardcode any of those strings in the solution of \\(\Search(p)\\).
- rewriting system
    - What does it mean that a rewriting system *computes* a function?
    - Given more time, a rewriting system can compute more functions.
    - A Turing machine is a rewriting system.
    - What is the difference between a rewriting system and a formal system?
    - What is the difference between a formal system and a formal language?
- Computation graph?
    - What should we name this sequence?
        - The sequence: \\( x, f(x), f(f(x)), \ldots, f^n(x), \ldots \\)
            - \\( x \\) is the initial state.
            - \\( f \\) is the next-state function.
        - Trace? History? Path? Computation path?
        - [WP:Iterated function](https://en.wikipedia.org/wiki/Iterated_function)
        - [WP:Iteration](https://en.wikipedia.org/wiki/Iteration)
- Function \\(f\\) is *computable* by formal system \\(S\\) iff \\(S\\) has a finite description of \\(f\\).
- Are there one-way functions?
- Mess: next-state relation
    - The _next-state relation_ \\(N\\) is obtained from \\(T\\)
    by making a loop for each isolated vertex.
    For each \\(x\\) in the domain of \\(T\\), \\(N(x,y) = T(x,y)\\).
    For each outside \\(x\\), \\(N(x,x)\\).
    - The computed relation of \\(T\\) is \\(N^\infty = N^\infty \circ N\\).
    - The problem computed by the graph is the infinite self-composition of the graph’s next-state function.
    Such problem is the smallest \\( X \\) that satisfies <span>\( N \circ X = X \)</span>.
    It is the least fixed point of \\( F \\) where <span>\( F(X) = N \circ X \)</span>.
    The nth self-composition of \\( N \\) is <span>\( N^n = E \circ N^{n-1} \)</span>.
    - An _infinite composition_ \\(N^\infty\\) is a relation satisfying \\(N^\infty \circ N = N^\infty\\).
    The empty relation satisfies this.
    The other one is nontrivial.
- Problem computed by a graph
    - A graph G *computes* the problem
    <span>\( P(G) = \{ (x,y) ~|~ \text{\(y\) is the nearest terminal vertex reachable from \(x\)} \} \)</span>.
    Because the graph is loopless, there is no path from a vertex to itself, a vertex is not reachable from itself.

```
terminal(x) = not exists y : E(x,y)
reach(x,y) = E(x,y) vee exists m ( reach(x,m) wedge reach(m,y) )
adist(x,y,1) = E(x,y)
adist(x,y,n) = exists m exists k : dist(x,m,k) wedge dist(m,y,n-k)
dist(x,y,n) = adist(x,y,n) wedge neg exists m < n : adist(x,y,m)
```

- Configuration graph as formal system?
    - Configuration *is* well-formed formula.
    - <span>\( E(a,b) \)</span> *is* iff <span>\( a' \vdash b' \)</span>.
    - Initial state *is* axiom.
    - <span>\( F \models P \)</span>

### Related articles

- related Wikipedia articles
    - [WP:Logic of graphs](https://en.wikipedia.org/wiki/Logic_of_graphs) (related to finite model theory)
    - [WP:Disjoint union](https://en.wikipedia.org/wiki/Disjoint_union)
- Introductory materials
    - Universal algebra (precursor to model theory)
        - [Peter Jipsen's "tutorial on universal algebra"](http://mathcs.chapman.edu/~jipsen/talks/BLAST2009/JipsenUAtutorial1pp.pdf)
presumes you know some abstract algebra.
        - [Matt Valeriote's "Lectures on universal algebra"](http://www.math.hawaii.edu/~ralph/Classes/619/UA-Valeriote.pdf)
        - Not so introductory?
            - [Joanna Grygiel's "universal algebra for logics"](http://www.uni-log.org/joana.pdf) (is this introductory?)
            - [An overview of modern universal algebra](http://www.math.hawaii.edu/~ralph/Classes/619/willard-ua.pdf)
            - [Applying Universal Algebra to Lambda Calculus](http://www.dsi.unive.it/~salibra/mainfinale.pdf)
    - Model theory
        - ["Fundamentals of model theory"](http://www.math.toronto.edu/weiss/model_theory.pdf)
    - Finite model theory
        - [Jan Van den Bussche's "Introduction to finite model theory"](https://dtai.cs.kuleuven.be/krr/files/seminars/IntroToFMT-janvdbussche.pdf)
        seems approachable
    - Hodge theory
        - [Vladimir G. Ivancevic and Tijana T. Ivancevic's "undergraduate lecture notes in de Rham--Hodge theory"](https://arxiv.org/abs/0807.4991)
- People and activities
    - [Erich Grädel](https://logic.rwth-aachen.de/~graedel/)
        - "Finite model theory studies the relationship between logical definability and computational complexity on finite structures."
        - [Algorithmic model theory](https://logic.rwth-aachen.de/Research/AlMoTh/)
        - ["Definability of summation problems for abelian groups and semigroups"](https://logic.rwth-aachen.de/pub/graedel/AbuzaidDawGraPak17.pdf).
"Our interest in the Abelian Semigroup Summation Problem
is due to the observation that it illustrates, in a mathematically
very pure way, the basic differences between logics and algorithms,
or between definability and complexity, that underly
some of the most fundamental and exciting problems of logic
in computer science."
        - ["The model-theoretic expressiveness of propositional proof systems"](https://logic.rwth-aachen.de/pub/graedel/GrPaPa17.pdf)
    - Ronald Fagin
    - Neil Immerman
    - many others?
- http://www.math.ucla.edu/~chernikov/teaching/17W-MATH285D/FiniteModelTheoryNotes.pdf
- external resources
    - online text books?
        - http://theoryofcomputing.org/articles/main/index.html
        - http://oajse.com/subjects/computer_science.html
    - journals?
        - https://www.hindawi.com/journals/ase/contents/
- Predicate Computation and Search Problem
    - Isomorphism:
        - Language ~ predicate.
        - Predicate ~ set.
            - (A predicate is a unary relation.)
    - Insight?
        - Every predicate <span>\(p\)</span>
        gives rise to at least two problems:
            - <span>\(\Compute(p) = \text{given \(x\), compute \(p(x)\)}\)</span>, and
            - <span>\(\Search(p) = \text{find any \(x\) such that \(p(x)\) is true}\)</span>.
        - https://en.wikipedia.org/wiki/Search_problem
    - There are \\(p\\) with slow \\(\Compute(p)\\) but fast \\(\Search(p)\\)?
    - There are \\(p\\) with fast \\(\Compute(p)\\) but slow \\(\Search(p)\\)?
    - Conjecture:
    For every complexity class \\(C\\),
    there exists \\(p\\) such that \\(\Compute(p) \in C\\).
    - Conjecture:
    For every predicate \\(p\\),
    there exists complexity class \\(C\\) such that \\(\Compute(p) \in C\\).
    - Conjecture:
    For every complexity classes \\(C \subset D\\),
    there exists \\(p\\) such that \\(\Compute(p) \in D-C\\).
    - Conjecture:
    For every complexity class \\(C\\),
    there exists \\(p\\) such that \\(\Compute(p) \in C\\) and \\(\Search(p) \not\in C\\).
    - Conjecture:
    For every predicate \\(p\\),
    there exists complexity class \\(C\\) such that \\(\Compute(p) \in C\\) and \\(\Search(p) \not\in C\\).
    - Problem formation conjecture:
    For every complexity class \\(C\\),
    there exists \\(p,P,Q\\) such that \\(P(p) \in C\\) and \\(Q(p) \not\in C\\).
    - Conjecture of arbitrary pessimization:
    For all complexity classes \\(F\\) and \\(S\\) where \\(F \subset S\\),
    there exists \\(p\\) such that \\(\Compute(p) \in S\\) and \\(\Search(p) \in F\\).
    The letters \\(F\\) and \\(S\\) are mnemonics for "fast" and "slow".
- Lemma: There exists fast \\(p\\) having slow \\(S(p)\\).
- Conjecture:
For every complexity classes \\(C\\),
there exists \\(p \in C\\) such that \\(S(p) \not\in C\\).
- Conjecture:
For every complexity classes \\(C\\) and \\(D\\),
there exists \\(p \in C\\) such that \\(S(p) \in D\\).
- Formal languages and describable problems
    - Consider the formal language \\(L\\)
    with alphabet <span>\( \{ z,s \} \)</span>
    and formation rules
    (1) \\( z \in L \\) and
    (2) \\( \alpha \in L \rightarrow s \alpha \in L \\).
    It should be obvious that \\(L\\) describes the set <span>\( \{ z, sz, ssz, \ldots \} \)</span>.
    This language solves the problem of adding a natural number by one.
- First-order logic
    - Example of terms in first-order logic:
        - \\(x\\)
        - \\(R(x,y)\\)
        - \\(R(x) \wedge S(y)\\)
