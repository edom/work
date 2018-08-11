---
title: Optimizing lambda calculus
date: 2018-08-11 23:36 +0700
permalink: /optlam.html
mathjax: yes
---

- http://thyer.name/lambda-animator/
- http://thyer.name/phd-thesis/
- http://hackage.haskell.org/package/graph-rewriting-lambdascope
- partial evaluation
    - 2010, slides, "O, partial evaluator, where art thou?", Lennart Augustsson, [pdf](http://www.cse.chalmers.se/~palka/Lennarts_talk/PEPM-2010.pdf)
        - https://en.wikipedia.org/wiki/Partial_evaluation
            - Futamura projection
    - 1997, article, "Distributed partial evaluation", [citeseerx](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.134.1238)
- Lambda calculus
    - might be related to bottom-up beta substitution
        - Abdullah hinted that BUBS (bottom-up beta-substitution [Shivers2004])
        might be used to make a garbage-free or a fast interpreter.
        - https://en.wikipedia.org/wiki/Strict_programming_language
        - https://en.wikipedia.org/wiki/Lazy_evaluation
        - [Strict-by-default vs Lazy-by-default](https://memo.barrucadu.co.uk/strict-vs-lazy.html)
        - https://en.wikipedia.org/wiki/Applicative_computing_systems
        - non-strict, beta reduction, normal order, applicative order
        - The terms "lazy" and "strict" imply operational semantics.
        They are two strategies for beta-reduction.
        "Lazy" is normal-order.
        "Strict" is applicative-order.
        - [An Algorithm for Optimal Lambda Calculus Reduction, John Lamping](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.2386&rep=rep1&type=pdf)
        - [Complete Laziness: a Natural Semantics, François-Régis Sinot](http://www.lsv.fr/Publis/PAPERS/PDF/sinot-wrs07.pdf)
        - http://rochel.info/ graph-rewriting-lambdascope (screenshot): An implementation of an optimal evaluator for the λ-calculus, PDFLambdascope
    - How is lambda calculus algebraic?
        - ["The lambda calculus is algebraic", Peter Selinger](https://www.mscs.dal.ca/~selinger/papers/combinatory.pdf)
            - "We argue that free variables should not be interpreted as elements in a model, as is usually done, but as indeterminates."
        - ["On the algebraic models of lambda calculus", Antonino Salibra](https://pdfs.semanticscholar.org/055d/69ee4dc95fbf6457419c90338493667478b1.pdf)
            - "The variety (equational class) of lambda abstraction algebras was introduced
            to algebraize the untyped lambda calculus in the same way Boolean algebras algebraize the classical propositional calculus."
            Propositional logic is modeled by Boolean algebra.
            First-order logic is modeled by cylindric algebra?
            Lambda calculus is modeled by lambda abstraction algebra.
            Why algebra? Because it is equational?
        - [Wikipedia "algebraic logic"](https://en.wikipedia.org/wiki/Algebraic_logic)
        - ["The algebraic lambda-calculus", Lionel Vaux](https://pdfs.semanticscholar.org/7596/19f05a42ff3045bcf87fcaa3edbff01e1130.pdf)
        - ["Lambda abstraction algebras: representation theorems", Don Pigozzi, Antonino Salibra](https://pdfs.semanticscholar.org/44c9/2ad00b8ceba78319005db048b24d61a80748.pdf)
        - ["Applying Universal Algebra to Lambda Calculus", Giulio Manzonetto, Antonino Salibra](http://www.dsi.unive.it/~salibra/mainfinale.pdf)
    - Dana Scott's PCF; also search the Internet for "the language pcf"
    ["Introduction to Real PCF (Notes)", Mart\'in H\"otzel Escard\'o](http://www.cs.bham.ac.uk/~mhe/papers/RNC3.pdf)
    - 1993 John Launchbury [Lazy imperative programming](https://pdfs.semanticscholar.org/492b/200419199892857faa6a6956614641ae9464.pdf)
- lambda calculus
    - Church-encoding enables lambda calculus to represent conditionals and algebraic data types.
    - Fixed-point combinators enables recursion and looping.
    - https://en.wikipedia.org/wiki/Lambda_cube
    - https://en.wikipedia.org/wiki/Calculus_of_constructions
    - https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
        - "The simply typed lambda calculus [...], a form of type theory,
        is a typed interpretation of the lambda calculus with only one type constructor: [...] that builds function types."
            - What is an "interpretation of the lambda calculus"?
            - What is "the lambda calculus"? Is there only one lambda calculus?
    - https://www.reddit.com/r/haskell/comments/8els6f/why_are_combinators_as_powerful_as_full/
    - https://math.stackexchange.com/questions/5639/the-power-of-lambda-calculi
    - Implement lambda calculus.
        - Without dynamic allocation / garbage collection.
        - Translate lambda calculus to assembly
            - Basic idea:
                - Every expression translates to a subroutine.
                - Calling the subroutine ~ evaluating the expression.
                - Subroutine return value ~ value obtained by evaluating the expression.
            - A lambda abstraction translates to a subroutine that accepts one parameter.
            - An application translates to a subroutine call.
            - An int value translates to what? Choice:
                - itself
                - a subroutine that returns the int
        - 2012, article, ["From Mathematics to Abstract Machine: A formal derivation of an executable Krivine machine"](https://arxiv.org/abs/1202.2924)
            - https://en.wikipedia.org/wiki/Krivine_machine
