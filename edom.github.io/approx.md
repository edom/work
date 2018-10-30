---
title: Approximating functions
permalink: /approx.html
date: 2018-08-17 22:52 +0700
mathjax: yes
---

We are interested in approximation theory because we want to justify how neural networks work.

- 2016, article, "Deep vs. shallow networks: An approximation theory perspective", [pdf available](https://arxiv.org/abs/1608.03287)
- [WP:Explainable Artificial Intelligence](https://en.wikipedia.org/wiki/Explainable_Artificial_Intelligence)

We should begin by skimming the 1998 book "A Short Course on Approximation Theory" by N. L. Carothers ([pdf](http://fourier.math.uoc.gr/~mk/approx1011/carothers.pdf)).
Then we should skim the 2017 lecture notes "Lectures on multivariate polynomial approximation" ([pdf](http://www.math.unipd.it/~demarchi/MultInterp/LectureNotesMI.pdf)).

The phrase "x *approximates* y" means "x is *close* to y", which implies distance, which implies metric space.

How close is the approximation?
Suppose that the function \\( g \\) approximates the function \\( f \\) in interval \\( I \\).
Then:

- The "approximation error at \\( x \\)" is \\( g(x) - f(x) \\).
- The "maximum absolute error" is \\( \max_{x \in I} \abs{g(x) - f(x)} \\).

How do we measure the distance between two \\( \Real \to \Real \\) functions \\( f \\) and \\( g \\)?
There are several ways.
Which should we use?

- The maximum norm, in interval \\( I \\) is \\( \max_{x \in I} \abs{f(x) - g(x)} \\).
This norm is also called uniform norm, supremum norm, Chebyshev norm, infinity norm, norm-infinity, \\( L_\infty \\)-norm.
Why is it called "uniform"?
[WP:Uniform norm](https://en.wikipedia.org/wiki/Uniform_norm).
- What is this norm called? \\( \int_{x \in I} [f(x)-g(x)]^2 ~ dx \\).

## Other

- Courses
    - 2017, [Approximation Theory, 7.5 ECTS](https://www.nada.kth.se/~olofr/Approx/)
    - 2012, syllabus, Drexel University, Math 680-002 (Approximation Theory), [pdf](http://www.math.drexel.edu/~foucart/TeachingFiles/S12/Math680Syl.pdf)
    - 2002, [MATH 5667-001: Introduction to Approximation Theory, CU-Denver, Fall 02](http://math.ucdenver.edu/~aknyazev/teaching/02/5667/).
- Subfields of approximation theory
    - Classical approximation theory deals with univariate real functions \\( \Real \to \Real \\).
    - Multivariate approximation theory deals with multivariate real functions \\( \Real^m \to \Real^n \\).
- Scenarios
    - Suppose we want to approximate the function \\( f \\),
    but we don't know the equation for \\( f \\);
    we only have a few input-output samples.
        - Can we approximate \\( f \\)?
        - How do approximation and curve-fitting relate?
- Overview
    - What is a multivariate polynomial?
    - Commonly conflated concepts
        - Approximation is not estimation.
            - Approximation converges.
            Estimation doesn't, because the actual value is unknown.
            - Approximation doesn't guess.
            Estimation does.
            - Approximation has error.
            Estimation has uncertainty.
            - Approximation is part of analysis.
            Estimation is part of statistics.
- The *uniform norm* is ...
- Best approximation is ...
- Uniform approximation is best approximation in uniform norm.
- https://en.wikipedia.org/wiki/Approximation_theory#Remez's_algorithm
    - https://en.wikipedia.org/wiki/Remez_algorithm
        - Inputs: a function, and an interval.
        - Output: an optimal polynomial approximating the input function in the input interval.
- What are Bernstein polynomials?
What question does the Weierstrass approximation theorem answer?
    - http://www4.ncsu.edu/~mtchu/Teaching/Lectures/MA530/chapter7.pdf
- [WP:Chebyshev polynomials](https://en.wikipedia.org/wiki/Chebyshev_polynomials)
    - Why is it important?
    How does it relate to best approximation?
        - "Chebyshev polynomials are important in approximation theory because the roots of the Chebyshev polynomials of the first kind, which are also called Chebyshev nodes, are used as nodes in polynomial interpolation.
        The resulting interpolation polynomial minimizes the problem of Runge's phenomenon and provides an approximation that is close to the polynomial of best approximation to a continuous function under the maximum norm."
- Machine learning as relation approximation
    - Machine learning, statistical modelling, function approximation, and curve fitting are related.
    - Generalize function approximation to relation approximation.
    - A function can be stated as a relation.
    - A relation can be stated as a function.
- Consider the least-square solution to an overdetermined system of linear equations.
Is such solution a kind of approximation?
    - There is no exact solution to begin with?
    - Why is it called "least-squares *approximation*"?
    - How can you approximate something that does not exist?
        - 1.2 approximates 1.23. Both 1.2 and 1.23 exist.
        - Contrarily, there is no X such that AX = B.
- What are approximation schemes?
    - https://en.wikipedia.org/wiki/Polynomial-time_approximation_scheme
- How do we approximate a function?
Is it even possible to approximate arbitrary functions?
    - If the function is analytic, we can truncate its Taylor series.
        - Commonly-used differentiable functions are analytic.
    - Chebyshev polynomials?
    - If we have an approximation scheme, we may be able to improve it.
        - https://en.wikipedia.org/wiki/Series_acceleration
            - https://en.wikipedia.org/wiki/Aitken%27s_delta-squared_process
    - google search: machine learning approximation theory
        - [Approximation Theory for Deep Learning Models: Where to Start? - Mathematics Stack Exchange](https://math.stackexchange.com/questions/2680158/approximation-theory-for-deep-learning-models-where-to-start)
        - http://www.vision.jhu.edu/tutorials/ICCV15-Tutorial-Math-Deep-Learning-Intro-Rene-Joan.pdf
        - 2017, slides, "From approximation theory to machine learning: New perspectives in the theory of function spaces and their applications", [pdf](http://npfsa2017.uni-jena.de/l_notes/vybiral.pdf)
        - 2018, article, "Approximation theory, Numerical Analysis and Deep Learning", [abstract](http://at.yorku.ca/c/b/p/g/30.htm)
            - "the problem of numerically solving a large class of (high-dimensional) PDEs (such as linear Black-Scholes or diffusion equations) can be cast into a classical supervised learning problem which can then be solved by deep learning methods"
- Determine whether we need to read these
    - Very likely
        - 2015, slides, "Best polynomial approximation: multidimensional case", [pdf](https://carma.newcastle.edu.au/meetings/spcom/talks/Sukhorukova-SPCOM_2015.pdf)
        - https://en.wikipedia.org/wiki/Bernstein_polynomial#Approximating_continuous_functions
            - https://en.wikipedia.org/wiki/Pointwise_convergence
            - https://en.wikipedia.org/wiki/Uniform_convergence
        - https://en.wikipedia.org/wiki/Approximation
            - https://en.wikipedia.org/wiki/Approximation_theory
                - is a branch of https://en.wikipedia.org/wiki/Functional_analysis
                - https://en.wikipedia.org/wiki/Approximation_theory#Chebyshev_approximation
            - https://en.wikipedia.org/wiki/Approximate_computing
                - example: https://en.wikipedia.org/wiki/Artificial_neural_network
        - https://en.wikipedia.org/wiki/Telescoping_series
    - Likely
        - 2018, slides, "Deep Learning: Approximation of Functions by Composition", [pdf](http://helper.ipam.ucla.edu/publications/dlt2018/dlt2018_14936.pdf)
            - classical approximation vs deep learning
        - 2013, short survey article draft, "Multivariate approximation", [pdf](http://num.math.uni-goettingen.de/schaback/research/papers/MultApp_01.pdf)
        - 1995, short introduction, "Multivariate Interpolation and Approximation by Translates of a Basis Function", [pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.2194&rep=rep1&type=pdf)
        - 1989, article, "A Theory of Networks for Approximation and Learning", [pdf available](http://www.dtic.mil/docs/citations/ADA212359)
            - What is the summary, especially about learning and approximation theory?
    - Unlikely
        - Survey-like
            - 2006, chapter, "Topics in multivariate approximation theory", [pdf available](https://www.researchgate.net/publication/226303661_Topics_in_multivariate_approximation_theory)
            - 1982, article, "Topics in multivariate approximation theory", [pdf](http://www.dtic.mil/dtic/tr/fulltext/u2/a116248.pdf)
            - 1986, "Multivariate Approximation Theory: Selected Topics", [paywall](https://epubs.siam.org/doi/book/10.1137/1.9781611970197)
        - Theorem
            - 2017, article, "Multivariate polynomial approximation in the hypercube", [pdf](https://people.maths.ox.ac.uk/trefethen/hypercube_published.pdf)
        - 2017, article, "Selected open problems in polynomial approximation and potential theory", [pdf](http://drna.padovauniversitypress.it/system/files/papers/BaranCiezEgginkKowalskaNagyPierzcha%C5%82a_DRNA2017.pdf)
        - 2017, article, "High order approximation theory for Banach space valued functions", [pdf available](https://ictp.acad.ro/jnaat/journal/article/view/1112)
        - Articles summarizing people's works
            - 2017, article, "Michael J.D. Powell’s work in approximation theory and optimisation", [paywall](https://www.sciencedirect.com/science/article/abs/pii/S0021904517301053)
            - 2000, article, "Weierstrass and Approximation Theory", [paywall](https://www.sciencedirect.com/science/article/pii/S0021904500935081)
        - 2013, article, "[1312.5540] Emerging problems in approximation theory for the numerical solution of nonlinear PDEs of integrable type", [pdf available](https://arxiv.org/abs/1312.5540)
        - 1985, article, "Some problems in approximation theory and numerical analysis - IOPscience", [pdf available](http://iopscience.iop.org/article/10.1070/RM1985v040n01ABEH003526)
        - 2011, article, "Experiments on Probabilistic Approximations", [pdf](https://people.eecs.ku.edu/~jerzygb/c154-clark.pdf)
- Less relevant overview
    - Why do we approximate?
        - Because it is practically inevitable.
            - Fundamental reason: Because computers are finite.
            - Practical reason: Trade-off between computation time and precision.
                - The more error we can afford, the faster we can run.
                    - May be related: 2013 monograph "Faster Algorithms via Approximation Theory" [pdf](http://theory.epfl.ch/vishnoi/Publications_files/approx-survey.pdf)
    - 2018 book "Recent Advances in Constructive Approximation Theory" [paywall](https://www.springer.com/us/book/9783319921648)
