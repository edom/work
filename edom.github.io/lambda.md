---
title: Extending lambda-calculus
permalink: /lambda.html
date: 2018-07-22 02:45 +0700
---

- TOC
{:toc}

## Introduction?

- unknown-year lecture notes "Lambda Calculus as a Programming Language" [pdf](http://andrei.clubcisco.ro/cursuri/2pp/01.Lambda_prog.pdf)

I thought lambda calculus could be summarized in one page, but Henk Barendregt wrote hundreds of pages about it. Is there more to lambda calculus than it seems?

- 1994, 50 pages, [pdf](http://www.nyu.edu/projects/barker/Lambda/barendregt.94.pdf)
- 1991, 190 pages, [pdf](https://people.mpi-sws.org/~dreyer/tor/papers/barendregt.pdf)

## Extending lambda-calculus with various bells and whistles

- Vectorial lambda-calculus
    - The 2013 article "The Vectorial Lambda-Calculus" [pdf](https://who.rocq.inria.fr/Alejandro.Diaz-Caro/TheVectorialCalculus.pdf) adds vectors and matrices and their types to lambda calculus.
    - The 2010 article "Semantics of a Typed Algebraic Lambda-Calculus" [pdf available](https://arxiv.org/abs/1006.1433) also mentions "vectorial".
- 2016 article "System F-omega with Equirecursive Types for Datatype-Generic Programming" [pdf](http://ps.informatik.uni-tuebingen.de/research/functors/equirecursion-fomega-popl16.pdf)

## Lambda calculus semantics?

- https://en.wikipedia.org/wiki/Lambda_calculus#Semantics
    - "In the 1970s, Dana Scott showed that, if only continuous functions were considered,
    a set or domain D with the required property could be found, thus providing a model for the lambda calculus."
        - 1982, ["What is a model of lambda calculus?"](https://www.sciencedirect.com/science/article/pii/S0019995882800879)
        - 2008, PhD thesis, ["Models and theories of lambda calculus"](https://tel.archives-ouvertes.fr/tel-00715207/document)
            - 2009, [summary](https://arxiv.org/abs/0904.4756)
- Paul Hudak, lecture notes, [The Lambda Calculus](http://www.cs.yale.edu/homes/hudak/CS430F07/LectureSlides/Reynolds-ch10.pdf)
    - "The Greatest Thing Since Sliced Bread™, or maybe even before it"
- The operational semantics of lambda calculus depends on the evaluation strategy?
    - What-reduction?
        - Normal-order reduction
        - Applicative-order reduction
    - Call-by-what?
        - Call-by-value
        - Call-by-name
