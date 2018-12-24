---
title: Computer science
date: 2018-04-15 00:00 +0700
permalink: /compsci.html
mathjax: yes
---

- Functional programming research
- [Matt Might: What CS majors should know](http://matt.might.net/articles/what-cs-majors-should-know/)
- [Wikipedia: List of important publications in computer science](https://en.m.wikipedia.org/wiki/List_of_important_publications_in_computer_science)
- Is CS a branch of math?
    - https://math.stackexchange.com/questions/649408/is-computer-science-a-branch-of-mathematics
    - http://www.scott-a-s.com/cs-is-not-math/
- How do we solve the P vs NP problem?
    - What is problem, computation, complexity, P, NP?
    - Can we construct a problem that is in NP but not in P?
    - Can we show that P = NP leads to contradiction?
- Undigested
    - [CSTSE 17502: What is a simple toy research programming language that has simple denotational semantics?](https://cstheory.stackexchange.com/questions/17502/a-simple-programming-language)
    - [Fast Approximate Matrix Multiplication by Solving Linear Systems](https://arxiv.org/abs/1408.4230)
    - [StackOverflow: Uses of algebraic structures in theoretical computer science](https://cstheory.stackexchange.com/questions/10916/uses-of-algebraic-structures-in-theoretical-computer-science)
    - [Danielsson et al.'s "Fast and loose reasoning is morally correct"](http://www.cse.chalmers.se/~nad/publications/danielsson-et-al-popl2006.html).
    - [StackOverflow: "Are there any applications of Abstract Algebra to Programming Language Theory? Is there anything that would be useful in language design and compiler implementation?"](https://cstheory.stackexchange.com/questions/12354/programming-language-theory-and-abstract-algebra)
    - [Math talk: Theorem about git revision control system?](https://cstheory.stackexchange.com/questions/32374/math-talk-theorem-about-git-revision-control-system)
    - http://www.ittc.ku.edu/~nfrisby/papers/drafts/Constructing_Language_Processors_with_Algebra_Combinators.pdf
        - modular monadic semantics?
- Programming language semantics
    - It is semantic incompatibility, not syntactic incompatibility,
    that makes programming languages hard to interoperate.
    - Semantic incompatibility is why we still don't have one programming language "to rule them all".
    - We don't even know the semantics of the programming language we use.
        - Even formalizing the semantics of C is hard.
    - In research programming languages, we start from the semantics, and then we realize that language on a computer.
    - In popular programming languages, we start with something already working, and then we try to formalize that language's semantics.
    - A semantic function maps a well-formed expression to a meaning (a denotation, a mathematical value).
    - [WP:Semantics](https://en.wikipedia.org/wiki/Semantics_(computer_science))
        - [[WP:Denotational semantics](https://en.wikipedia.org/wiki/Denotational_semantics)]
            - "An important tenet of denotational semantics is that semantics should be compositional:
            the denotation of a program phrase should be built out of the denotations of its subphrases."
                - This needs an example.
    - Some readings about denotational semantics:
        - https://web.eecs.umich.edu/~weimerw/2008-615/lectures/weimer-615-07.pdf
        - https://www.cs.colorado.edu/~bec/courses/csci5535/reading/densem.pdf
        - https://www.cl.cam.ac.uk/~gw104/dens.pdf
- [Goguen's Semantics of Computation research page](http://cseweb.ucsd.edu/~goguen/projs/sem.html)
- Programming language research
    - Is there something like Pandoc but for programming language source-to-source translation?
    Software reengineering tools?
    Eclipse Modeling Framework?
    LLVM?
    - 2018-06-27 an idea about coercive subtyping with "maximally free" coercion function
        - "\\( A \le B \\)" means "\\( A \\) is a subtype of \\( B \\)".
        - \\( A \\) is a subtype of \\( B \\) iff we can write a *maximally free* function \\( g : A \to B \\).
        - What is a *maximally free* function?
            - It doesn't use anything more than parameters, data constructors, abstraction, and application.
            - It is the one that uses the most parameters among all free functions with the same type.
            - Examples:
                - \\( x \mapsto x + 1 \\) is not free for any types because it uses + and 1.
                - \\( (x,y) \mapsto x \\) for type \\( \forall a, b : (a,b) \to a \\)
                is free because it's the only possible function
                - \\( x \mapsto Just(x) \\) for type \\( \forall a : a \to Maybe(a) \\) is free.
                - \\( x \mapsto Nothing \\) for that type is also free, but not maximal,
                but is maximally free for \\( \forall a, b : a \to Maybe(b) \\).
                - How about \\( a <_? (a,a) \\)?
                There are more than one maximally free function with that type.
                Thus \\( a \\) is *not* a subtype of \\( (a,a) \\).
                But \\( a \\) is a subtype of \\( (a,b) \\) if \\( a \not< b \\).
                This is strange?
        - Example:
            - Every type \\( a \\) is a subtype of \\( (a,b) \\) because the maximally free coercion function is unambiguous: \\( (x,y) \mapsto x \\).
            - Every type \\( a \\) is a subtype of \\( Maybe(a) \\) because the maximally free coercion function is unambiguous: \\( x \mapsto Just(x) \\).
            - Let \\( f : Type \to Type \\) be a functor.
            Then every type \\( a \\) should be a subtype of \\( f(a) \\)
            with the coercion function \\( x \mapsto Pure(x) \\).
        - It's coercive subtyping, but the coercion function has to be free.
            - A la Haskell Djinn, for a given type, there should be only one such free function.
            - https://en.wikipedia.org/wiki/Subtyping#Coercions
        - https://en.wikipedia.org/wiki/Liskov_substitution_principle
    - Hierarchy of type systems?
        - Hierarchy of lambda calculuses?
            - https://cstheory.stackexchange.com/questions/8234/is-there-an-expressiveness-hierarchy-for-type-systems
            - https://en.wikipedia.org/wiki/Lambda_cube

    - What are the refinements of System F?
        - https://en.wikipedia.org/wiki/System_F
        - What is System F-omega?
        - What is System F with subtyping?
- Unread
    - https://lukepalmer.wordpress.com/2013/03/12/constructions-on-typeclasses-part-1-f-algebras/
    - K language http://fsl.cs.illinois.edu/index.php/Programming_Language_Design_and_Semantics
    - https://en.wikipedia.org/wiki/Refal
    - https://en.wikipedia.org/wiki/Extensible_programming
- Is there a semantics-first/semantics-driven programming language design?
- https://github.com/qorelanguage/qore
- https://github.com/PistonDevelopers/dyon
- https://cburgdorf.wordpress.com/2014/07/17/rust-will-be-the-language-of-the-future/
- http://www.evanmiller.org/why-im-betting-on-julia.html
- http://zverovich.net/2016/05/13/giving-up-on-julia.html
- https://software-carpentry.org/blog/2015/06/why-i-am-not-excited-about-julia.html
- http://wesmckinney.com/blog/why-im-not-on-the-julia-bandwagon-yet/
- http://www.oceanographerschoice.com/2016/03/the-julia-language-is-the-way-of-the-future/
- [Wolfram language](https://www.wolfram.com/language/)
- [Terra: low-level Lua](http://terralang.org/)
- https://en.m.wikipedia.org/wiki/Robert_W._Floyd
    - Assigning meaning to programs
- [Semantics of programming languages, Benjamin Pierce](http://www.allisons.org/ll/Semantics/)
- Haxe
    - https://stackoverflow.com/questions/5329940/meta-programming-write-in-one-language-x-cross-compile-in-multiple-languages-l
        - "Haxe is a sort of meta-language that compiles for different platforms:"
    - https://haxe.org/documentation/introduction/language-features.html
- [2002 Featherweight Java: A Minimal Core Calculus for Java and GJ](https://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf)
- [The principal programming paradigms](https://www.info.ucl.ac.be/~pvr/paradigmsDIAGRAMeng108.pdf)
(PDF poster).
- ["Data-centric Metaprogramming in Object-Oriented Languages"](https://2015.ecoop.org/event/icooolps-2015-papers-co-developing-libraries-and-their-optimizations)
- https://en.wikipedia.org/wiki/Industrial_PC
- https://en.wikipedia.org/wiki/Rugged_computer
- https://en.wikipedia.org/wiki/Embedded_system
- https://en.wikipedia.org/wiki/Industry_4.0
- We also have this "Shadow IT" problem:
    - https://en.wikipedia.org/wiki/Shadow_IT
    - https://community.atlassian.com/t5/Confluence-questions/Best-practice-Using-confluence-and-other-systems-like-yammer/qaq-p/323867
- [Kruna Ratkovic, "Notion of Strong Monad in Computing"](https://www.researchgate.net/profile/Kruna_Ratkovic/publication/323426688_Notion_of_Strong_Monad_in_Computing/links/5a958819a6fdccecff090883/Notion-of-Strong-Monad-in-Computing.pdf)
- quantum computing
    - should this be in comp-sci or in physics?
    - 2018, industry practitioner interview
        - [YT:Current State of Quantum Computing - Computerphile](https://www.youtube.com/watch?v=PN7mPYcWFKg)
        - [YT:Quantum Instruction Set - Computerphile](https://www.youtube.com/watch?v=ZN0lhYU1f5Q)
    - [Gil Kalai is skeptical about quantum computers](https://www.quantamagazine.org/gil-kalais-argument-against-quantum-computers-20180207/)
- [Anatomy of Programming Languages](https://www.cs.utexas.edu/~wcook/anatomy/anatomy.htm), what is this?
- [Stephen Diehl, Write you a Haskell, Building a modern functional compiler from first principles](http://dev.stephendiehl.com/fun/)
