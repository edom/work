---
title: Making the only programming language we will ever need
permalink: /plres.html
date: 2018-07-22 02:45 +0700
mathjax: true
---

- TOC
{:toc}

## Goal

The goal is to make *the* programming language.

A goal of programming language research is to make a better programming language (if not the best).
Do more with less.

*The* ultimate best programming language?

## Subtopics

- [Extending lambda calculus]({% link lambda.md %})
- [Optimizing lambda calculus]({% link optlam.md %})
- [Extending Haskell]({% link exhask.md %})
- [Programming language design mistakes]({% link pldm.md %})
- [Designing markup languages]({% link markup.md %})
- [Designing configuration languages]({% link conflang.md %})
- Making interpreters/translators/compilers
    - Don't make a compiler?
    Make an interpreter instead, and stage it?
    Turn an interpreter into a compiler for free?
    - [Parsing]({% link parse.md %})

## What is a program?

A program is an executable formal system.

Every functional programming language is lambda calculus plus plus.

Functional programming is lambda calculus plus plus.

## What should we know before creating a programming language?

- 2015 dissertation "Evidence-based programming language design: a philosophical and methodological exploration",
[abstract](https://jyx.jyu.fi/handle/123456789/47698),
[pdf](https://jyx.jyu.fi/bitstream/handle/123456789/47698/978-951-39-6388-0_vaitos04122015.pdf)
- [What are some interesting language features that may not be well known? : ProgrammingLanguages](https://www.reddit.com/r/ProgrammingLanguages/comments/8vcrzb/what_are_some_interesting_language_features_that/)
- [A practitioner’s guide to reading programming languages papers](https://blog.acolyer.org/2018/01/26/a-practitioners-guide-to-reading-programming-languages-papers/)
    - from [top scoring links : ProgrammingLanguages](https://www.reddit.com/r/ProgrammingLanguages/top/)

## What are some interesting programming languages?

- Interesting functional programming languages tailored for web programming.
Perhaps related to data modeling.
    - Ur/Web
        - [FAQ](http://www.impredicative.com/ur/faq.html)
            - "Why would I prefer to use Ur/Web over OPA?"
        - [How to Get Started Learning Ur/Web](http://www.impredicative.com/ur/resources.html)
    - [The Links Programming Language](http://links-lang.org/)
    - [The Opa Language](http://opalang.org/)
- Moving logic into SQL stored procedures
    - [Simplify: move code into database functions \| Derek Sivers](https://sivers.org/pg)
        - A legitimate concern: How do we version-control (and release, and rollback) stored procedures, triggers, and other database logics?
    - [Andl, a relational language that is not SQL, is coming to Postgres \| Hacker News](https://news.ycombinator.com/item?id=11802917)
- https://www.microsoft.com/en-us/research/publication/convenient-explicit-effects-using-type-inference-with-subeffects/
- [If Haskell were strict, what would the laziness be like?](https://nikita-volkov.github.io/if-haskell-were-strict/)
- http://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt

### Do we really have to read these fragmented sources?

- [Lecture 12: Design and Evolution of Programming Languages](http://www.cse.chalmers.se/edu/year/2015/course/DAT150/lectures/proglang-12.html)

## Increasing language adoption

In order for a language to be adopted, people must perceive its risk as low.

The language must work with existing codebases.

The language designer must think from the language user's point of view.
Let's say I have 100,000 lines of Java that I've been writing and testing for the past 5 years.
Are you expecting me throw away all of them?

Thus the language must work with C, C++, C#, Java, Go, JavaScript, Python, Ruby, and everything else.
This should be possible because the essence of all programming languages is the same: every programming language is a formal system.
It should be possible to translate a program P1 in language L1 to program P2 in language L2 with the same semantics.

Improve/enhance, not supersede.

Mixing languages should be easy.

2013, article, "Empirical analysis of programming language adoption", [pdf](http://sns.cs.princeton.edu/docs/asr-oopsla13.pdf)

The language must be suitable for systems programming.
- System programming is hardware-aware programming.
Application programming assumes abstract machine, infinite memory, and all convenience provided by the operating system.
    - Why do we make this distinction?

The language must facilitate metaprogramming.
Everything must be a first-class citizen.
It has to have EVAL.
The language must provide a way for interpreting/compiling/loading a program at runtime.
The compiler becomes a part of every program.

What is the reason for the name "metacircular evaluator"?
What is circular?
What is metacircular?

To make syntax first-class, we need QUOTE and UNQUOTE (such as in Lisp/Scheme)?

To prevent syntax flamewar, we should define the canonical linearization of the abstract syntax tree.
Go does this with `go fmt`.
I think that is wise.

- Basic assumptions
    - Computer (machine) is embodied formal system.
        - Assume no hardware fault.
    - Software is executable mathematics.

## Finding giants whose shoulders we want to stand on

- These languages are interesting starting points:
    - [Sixten](https://github.com/ollef/sixten)
    - Dhall
    - [Morte: an intermediate language for super-optimizing functional programs](http://www.haskellforall.com/2014/09/morte-intermediate-language-for-super.html)
- Designing programming languages:
    - 2018 article "Interdisciplinary Programming Language Design" [pdf](http://www.cs.cmu.edu/~mcoblenz/HCPLD-preprint.pdf)
    - 2018 article "A Programmable Programming Language" [pdf](http://silo.cs.indiana.edu:8346/c211/impatient/cacm-draft.pdf)
    - 2018 article "Combining Model Checking and Testing" [pdf](https://www.cis.upenn.edu/~alur/CIS673/testing.pdf)
    - 2013 article "Idris, a General Purpose Dependently Typed Programming Language: Design and Implementation" [pdf](https://eb.host.cs.st-andrews.ac.uk/drafts/impldtp.pdf)
    - 2007 article "Position Paper: Thoughts on Programming with Proof Assistants" [pdf](https://ac.els-cdn.com/S1571066107002502/1-s2.0-S1571066107002502-main.pdf?_tid=19c6192b-ca57-4ef0-9403-1cfb624c817c&acdnat=1535488824_78a2c31f390f3a1fb72f0c31024f2495)
    - 1996 article "Seven Deadly Sins of Introductory Programming Language Design" [pdf](https://pdfs.semanticscholar.org/d314/78c5b95c399b6418b41011debefbc699c633.pdf)
    - 1996 book "Advanced programming language design" [pdf](https://www.researchgate.net/profile/Raphael_Finkel/publication/220692467_Advanced_programming_language_design/links/0c96052af3e324bf31000000/Advanced-programming-language-design.pdf)
    - 1973 article "Hints on Programming Language Design" [pdf](http://www.dtic.mil/dtic/tr/fulltext/u2/773391.pdf)
- Finding recent programming language research:
    - meetings, conferences, symposiums
        - [POPL on Twitter](https://twitter.com/poplconf?lang=en).
        Its full name is "ACM SIGPLAN Symposium on Principles of Programming Languages".
    - collections, links, aggregators
        - https://www.cs.cmu.edu/~mleone/language-research.html
- What already exists?
    - [68 Resources on creating programming languages](https://tomassetti.me/resources-create-programming-languages/)
    - 2004, book, "Programming language design concepts"
    - https://en.wikipedia.org/wiki/Expression_problem
    - don't repeat yourself
        - https://en.wikipedia.org/wiki/Generic_programming
            - parametric polymorphism
    - Safely Composable Type-Specific Languages https://www.cs.cmu.edu/~aldrich/papers/ecoop14-tsls.pdf
- books
    - books recommended by courses related to programming language research
        - http://plus.kaist.ac.kr/~han/courses/cs520/
        - https://www.cl.cam.ac.uk/teaching/1516/ConceptsPL/
        - https://en.wikipedia.org/wiki/Programming_language_theory#Further_reading
    - 1995, book, "Syntax and semantics of programming languages", Slonneger & Kurtz http://homepage.divms.uiowa.edu/~slonnegr/plf/Book/
- 2017, PhD thesis, ["Context-aware programming languages"](http://tomasp.net/academic/theses/coeffects/)
    - [amazingly empathic considerate presentation that has the audience in mind](http://tomasp.net/coeffects/)
        - if only all PhD theses are presented that way
            - if only everybody writes that way
- 2014, book, Odersky, "Scala by example"
    - perhaps the best way to learn Scala for non-Scala programmers
- https://en.wikipedia.org/wiki/John_C._Reynolds
    - 2014, [The essence of Reynolds](http://www.cs.bham.ac.uk/~udr/papers/EssenceOfReynolds.pdf)
        - 1998, Reynolds, "Theories of programming languages"
- a list of people http://www.angelfire.com/tx4/cus/people/index.html
- other people's collections
    - https://github.com/steshaw/plt

### People who share the vision for *the* programming language

- [Polymorphic Programming - BAM Weblog](https://brianmckenna.org/blog/polymorphic_programming)
    - has some links
    - 2017, article, "Compiling to Categories", Conal Elliott, [pdf](http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf)
    - Is this 1969 article related? Can't be found online.
        - https://en.wikipedia.org/wiki/Polymorphic_Programming_Language
    - Are these related?
        - 1989, article, "Database Programming in Machiavelli – a Polymorphic Language with Static Type Inference", [pdf](http://homepages.inf.ed.ac.uk/opb/papers/SIGMOD89.pdf)
- Can we use Description Logic (the theoretical foundations of Web Ontology Language OWL) to generate software, especially enterprise applications?
    - 2015, article, [[1503.01723] Modelling the Semantic Web using a Type System](https://arxiv.org/abs/1503.01723)
    - 2005, article, "Software engineering ontologies and their implementation", [pdf](https://espace.curtin.edu.au/bitstream/handle/20.500.11937/9549/19598_downloaded_stream_116.pdf), [pdf copy](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.86.5585&rep=rep1&type=pdf)

### People who share some of the vision but don't go far enough

- XForms
    - 2016, article, [Leveraging declarative languages in web application development - SpringerLink](https://link.springer.com/article/10.1007/s11280-015-0339-z)
    - 2014, slides, advocacy, [Declarative Web Applications](https://homepages.cwi.nl/~steven/Talks/2014/01-31-declarative/)
- 2007, workshop, [Declarative Models of Distributed Web Applications](https://www.w3.org/2007/02/dmdwa-ws/)
- [#proglangdesign](http://www.proglangdesign.net/)
- IDE (integrated development environment), editor
    - Given a grammar, the computer should generate an IDE for us.
    Syntax highlighting.
    Refactoring.
    - https://hackage.haskell.org/package/treersec "Build a structure editor for a programming language from combinators following the grammar."
    - http://www.lamdu.org/
    - cirru https://news.ycombinator.com/item?id=13773813
        - lots of similar things https://news.ycombinator.com/item?id=13774864
    - isomorf: find code fragment popularity by structure (not-only-text) comparison https://isomorf.io/#!/tours/of/overview/7/haskell
- supercompilation, specialization, partial evaluation
    - Haskell supercompilation?
        - GHC
        - Supero
        - Morte
            - https://github.com/Gabriel439/Haskell-Morte-Library
                - "Morte is a super-optimizing intermediate language for functional languages."
            - http://www.haskellforall.com/2014/09/morte-intermediate-language-for-super.html
    - [Thyer's PhD thesis "Lazy specialization"](http://thyer.name/phd-thesis/thesis-thyer.pdf) has an accessible introduction to lambda calculus in Chapter 2.
        - "Tower of interpreters" test
        - 2018, "Collapsing towers of interpreters" http://lampwww.epfl.ch/~amin/pub/collapsing-towers.pdf
            - "It is well known that *staging* an interpreter – making it generate code whenever it would normally interpret an expression – yields a compiler [...]"
    - 1991, "A partial evaluator for the untyped lambda-calculus", [paywall](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/a-partial-evaluator-for-the-untyped-lambda-calculus/EE324F936F0A009B6766B13FF6755DFC)
        - related: semantic-directed code generation?
    - Gabriel Gonzales stuff: Morte, Dhall.
    - [LTU: Ongoing work on Supercompilation of Java code (or supercompilation in general)?](http://lambda-the-ultimate.org/node/2739)
- [A Treatise on Cosmos —the New Programming Language](https://medium.com/@McCosmos/a-treatise-on-cosmos-the-new-programming-language-905be69eb4af)
    - procedural-looking logic programming language
- syntax
    - [WP:Off-side rule](https://en.wikipedia.org/wiki/Off-side_rule), indentation as block delimiter
- [Quest For The Perfect Language](http://wiki.c2.com/?QuestForThePerfectLanguage)

## Category theory and programming languages?

The section title needs a verb.

- Category-theoretic model of functional programming languages
    - Every functional programming language L can be modeled by a category C(L) whose objects are the types of L and arrows are the function expressions of L.
- categorical programming (what is this?)
    - 2000, PhD thesis, "Categorical programming with inductive and coinductive types" https://kodu.ut.ee/~varmo/papers/thesis.pdf
    - categorical programming language
        - 1993, article, "Comparing Hagino's categorical programming language and typed lambda-calculi" https://www.sciencedirect.com/science/article/pii/030439759390186W
        - 1987, PhD thesis, "Categorical programming language" http://web.sfc.keio.ac.jp/~hagino/thesis.pdf
            - "An interpreter of Hagino's Categorical Programming Language (CPL)." https://github.com/msakai/cpl
        - aggregators
            - 2009, https://mathoverflow.net/questions/3721/programming-languages-based-on-category-theory
            - https://softwareengineering.stackexchange.com/questions/216635/category-theory-based-language
- category theory applied to programming language theory
    - 2012, "Generic Programming with Adjunctions" http://www.cs.ox.ac.uk/ralf.hinze/LN.pdf

## Making compilers?

Every compiler does name resolution / symbol table.
Is there a compiler that doesn't do that?
[Forth?](https://www.reddit.com/r/Forth/comments/695oik/advances_in_forth_language_design/dh454oq/)

- https://www.reddit.com/r/haskell/comments/4jhhrj/anders_hejlsberg_on_modern_compiler_construction/
- https://cs.stackexchange.com/questions/63018/visual-programming-tools-why-don-t-they-work-with-the-ast-directly
- compiling with continuations
    - Why use CPS (continuation passing style) as intermediate form?
        - http://matt.might.net/articles/cps-conversion/
        - https://www.microsoft.com/en-us/research/publication/compiling-with-continuations-continued/
        - https://news.ycombinator.com/item?id=7150095
    - 2003, retrospective: the essence of compiling with continuations https://users.soe.ucsc.edu/~cormac/papers/best-pldi.pdf
        - https://en.wikipedia.org/wiki/A-normal_form

## Foundation of mathematics especially for programming?

- set theories, such as ZF, ZFC, NBG, etc.
    - https://math.stackexchange.com/questions/136215/difference-between-zfc-nbg
- type theories, such as Martin-Löf type theory
- logic?
- category theory?
- lambda calculus?
- https://cstheory.stackexchange.com/questions/27217/are-there-presentations-of-set-theory-in-terms-of-lambda-calculus
    - Grue's map theory, 1992
        - http://hjemmesider.diku.dk/~grue/
            - 1992, Grue, PhD thesis, ["Map theory"](http://hjemmesider.diku.dk/~grue/papers/Grue92/Grue92.pdf)
                - p. 130: "Equality is better than truth [...]" (How do we explain the context?)
                    - https://en.wikipedia.org/wiki/Equational_logic
        - related?
            - equational programming
        - 2016 reformulation article "A synthetic axiomatization of Map Theory" [pdf available](https://hal.archives-ouvertes.fr/hal-00678410v3)

Can we formalize "a program is an executable formal system" using Grue's map theory?

- How is "false" represented?
- How is "true" represented?
- How is "conjunction" represented?

## Paradigm, approach, viewpoint, worldview?

- graph programming languages
    - https://cstheory.stackexchange.com/questions/3906/what-are-theoretically-sound-programming-languages-for-graph-problems
        - https://www.cs.york.ac.uk/plasma/wiki/index.php?title=GP_%28Graph_Programs%29
        - 2007, PhD thesis, Steinert, ["The graph programming language GP"](https://www.cs.york.ac.uk/ftpdir/reports/2007/YCST/15/YCST-2007-15.pdf)
        - a short visual example of "conditional rule schemata"
            - 2010, article, ["Hoare Logic for Graph Programs"](https://www.cs.york.ac.uk/plasma/publications/pdf/PoskittPlump.VS-Theory.10.pdf)
        - https://markorodriguez.com/2013/01/09/on-graph-computing/
        - https://en.wikipedia.org/wiki/Gremlin_(programming_language)
- equational programming?
    - 2017-2018, https://www.cs.vu.nl/~tcs/ep/
    - term-rewriting
        - retired, [Q language](http://q-lang.sourceforge.net/)
            - http://q-lang.sourceforge.net/examples.html
            - superseded by Pure
                - https://agraef.github.io/pure-lang/
                - https://en.wikipedia.org/wiki/Pure_(programming_language)
                - https://github.com/agraef/pure-lang/wiki/Rewriting
        - Joy
- 2002, article, ["Stochastic Lambda Calculus and Monads of Probability Distributions"](http://www.cs.tufts.edu/comp/150PP/archive/norman-ramsey/pmonad.pdf)
- "Purely functional lazy nondeterministic programming", [paywall](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/purely-functional-lazy-nondeterministic-programming/1E8BA117E549A9612BC4AF9804E5507A)
- relational programming (pure logic programming?)
    - miniKanren
        - Byrd PhD thesis https://scholarworks.iu.edu/dspace/bitstream/handle/2022/8777/Byrd_indiana_0093A_10344.pdf
            - mentions other programming languages: Prolog, Mercury, Curry

## Toward a language with first-class syntax?

- composable grammars?
    - 2013, article, Viera & Swierstra, "First Class Syntax, Semantics, and Their Composition" http://www.cs.ru.nl/P.Achten/IFL2013/symposium_proceedings_IFL2013/ifl2013_submission_21.pdf
        - 2013, PhD thesis, Viera, "First Class Syntax, Semantics, and Their Composition" file:///home/erik/Downloads/viera.pdf
    - 1990, ["A Primer for Program Composition Notation"](https://authors.library.caltech.edu/26726/)
    - https://stackoverflow.com/questions/953185/composable-grammars
    - OMeta
    - Programming languages with programmable syntax
        - [Katahdin](http://chrisseaton.com/katahdin/)
    - parsing expression grammar, packrat
        - "Parsing ought to be easier"
            - https://news.ycombinator.com/item?id=2330830
                - "PEGs are one class of cleanly composable grammars."?
    - http://www.cs.cmu.edu/~Compose/
    - cryptographic protocol analysis
        - https://en.wikipedia.org/wiki/Universal_composability
- Programming languages with macros
    - Common Lisp
    - Scheme
    - Kotlin?
    - Clojure?
    - Scala? https://www.scala-lang.org/blog/2017/11/27/macros.html

## Type systems and type theory?

- https://cs.stackexchange.com/questions/53998/what-are-the-major-differences-between-row-polymorphism-and-subtyping
- row polymorphism vs structural subtyping https://news.ycombinator.com/item?id=13047934
    - Why is it called row polymorphism?
    Where is the row?
        - See "Objects and Aspects: Row Polymorphism", Neel Krishnaswami https://www.cs.cmu.edu/~neelk/rows.pdf
    - Church types and Curry types
        - https://lispcast.com/church-vs-curry-types/
    - Liquid Haskell https://ucsd-progsys.github.io/liquidhaskell-blog/
    - [Row Polymorphism Isn't Subtyping - BAM Weblog](https://brianmckenna.org/blog/row_polymorphism_isnt_subtyping)
    - recursive types http://ecee.colorado.edu/~siek/ecen5013/spring10/lecture15.pdf
    - http://tomasp.net/coeffects/
    - 2008, article, "Church and Curry: Combining Intrinsic and Extrinsic Typing", Frank Pfenning https://www.cs.cmu.edu/~fp/papers/andrews08.pdf
    - From Wadler home page
        - [From Data Types to Session Types: A Basis for Concurrency and Distribution](http://groups.inf.ed.ac.uk/abcd/)
    - [Vectors are records, too (pdf) : dependent_types](https://www.reddit.com/r/dependent_types/comments/8qig0u/vectors_are_records_too_pdf/)
    - https://wiki.haskell.org/Untypechecking is "converting from a type to a term".
    - NOOL 2016 article "Static Typing Without Static Types — Typing Inheritance from the Bottom Up" [pdf](http://www.it.uu.se/workshop/nool16/nool16-paper4.pdf)
- Every recursive type can be written as `mu a. F a` where F is the associated free functor?
    - Example: `List a = Fix (F a)` where `F a b = 1 + a * b`.
    - `Fix F = F (Fix F)` is the least fixed point of F.
- Why should we care about parametricity?
    - [Parametricity: Money for Nothing and Theorems for Free - Bartosz Milewski's Programming Cafe](https://bartoszmilewski.com/2014/09/22/parametricity-money-for-nothing-and-theorems-for-free/)

### What is a type?

- Mathematically, A type is a set whose membership is syntactically decidable.
- Engineeringly, A type gives meaning to bit patterns.
A type encodes meaning into bit patterns.
    - For example, consider the bit pattern 11111111.
        - It means -1 as an int8.
        - It means 127 as an uint8.

### Type-checking

- Every type checking algorithm is unification (logic programming).
    - If the programmer annotates a term, use that annotation.
    - If the a term is not annotated, annotate that term with a freshly generated variable.
    - Unify everything according to the typing rules.
    - See Algorithm W in [WP:Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).

### Algebraic subtyping?

- subtyping
    - algebraic subtyping
        - 2016, PhD thesis, "Algebraic subtyping", Stephen Dolan https://www.cl.cam.ac.uk/~sd601/thesis.pdf
        - "Polymorphism, subtyping and type inference in MLsub" http://www.cl.cam.ac.uk/~sd601/papers/mlsub-preprint.pdf
            - from http://lambda-the-ultimate.org/node/5393
                    - from https://contributors.scala-lang.org/t/algebraic-subtyping/577
- http://www.cl.cam.ac.uk/~sd601/papers/mlsub-preprint.pdf
- https://www.cl.cam.ac.uk/~sd601/thesis.pdf
- https://cs.stackexchange.com/questions/53998/what-are-the-major-differences-between-row-polymorphism-and-subtyping
- https://www.cl.cam.ac.uk/teaching/1415/L28/rows.pdf
- 2017, "Introduction to homotopy type theory", http://www.cs.nott.ac.uk/~psztxa/ss-types-17/notes-summer17.pdf
- http://www.cs.nott.ac.uk/~psztxa/ss-types-17/
- https://homotopytypetheory.org/book/
- https://en.wikipedia.org/wiki/Subtyping
- https://en.wikipedia.org/wiki/Circle-ellipse_problem
- What does "X is a Y" mean?
It means P(X) => P(Y) for every predicate P.
Liskov substitution principle.
Does it mean "X is a subclass of Y" or "X is an instance of Y"?

## Drinking from the firehose

- Composition, composable systems
    - https://en.wikipedia.org/wiki/Software_transactional_memory#Composable_operations
- https://en.wikipedia.org/wiki/Programming_language_theory
- refactoring
    - [github.com/PyCQA/baron](https://github.com/PyCQA/baron): "IDE allow you to refactor code, Baron allows you to write refactoring code."
    - HaRe Haskell refactoring tool https://github.com/alanz/HaRe
- ungrouped
    - https://medium.com/generative-design/introduction-to-computational-design-6c0fdfb3f1
    - magic?
        - http://conal.net/blog/posts/semantic-editor-combinators
    - [Salon des Refusés 2017](https://2017.programmingconference.org/track/refuses-2017)
- 2002, article collection, "Recent advances in Java technology: theory, application, implementation" http://www.cs.nuim.ie/~jpower/Research/Papers/2002/power-raijt-toc.pdf
- 1985, article, "Automatic synthesis of typed Λ-programs on term algebras" https://www.sciencedirect.com/science/article/pii/0304397585901355
- 2015, article, "Dynamically Composing Languages in a Modular Way: Supporting C Extensions for Dynamic Languages", [pdf](https://chrisseaton.com/rubytruffle/modularity15/rubyextensions.pdf)
- https://github.com/nim-lang/Nim
- 2016, article, "Towards Ontology-Based Program Analysis", [pdf](http://drops.dagstuhl.de/opus/volltexte/2016/6120/pdf/LIPIcs-ECOOP-2016-26.pdf)
    - Interesting conference title: "Conference on very important topics (CVIT)"
        - Is it real?
        Is it a secret society?
        Google doesn't seem to know about it.
- functional languages with explicit memory layout?
functional languages for systems programming?
    - [Sixten: Functional programming with fewer indirections](https://github.com/ollef/sixten)
        - It also deals with representing algebraic data type inhabitants as bit patterns.
        - "Sixten is very related to other functional languages such as Haskell, Agda, and Idris.
        The biggest difference between other languages and Sixten is the way that Sixten allows us to control the memory layout of data."
        - [Sixten, "General", Gitter, community chat](https://gitter.im/sixten-lang/General?source=orgpage)
- What are Prolog alternatives?
    - 2011, article, [[1107.5408] A structured alternative to Prolog with simple compositional semantics](https://arxiv.org/abs/1107.5408)
- 2017, article, [[1707.00024] A Formalized General Theory of Syntax with Bindings](https://arxiv.org/abs/1707.00024)
- [Programming Language and Compiler Research Groups](https://www.cs.cmu.edu/~mleone/language/projects.html)
- [IEEE posts its top list of languages - The PL Enthusiast](http://www.pl-enthusiast.net/2014/07/10/ieee-posts-its-top-list-of-languages/)
- [What is PL research and how is it useful? - The PL Enthusiast](http://www.pl-enthusiast.net/2015/05/27/what-is-pl-research-and-how-is-it-useful/)
- 2014, article, [Ontology-based Representation and Reasoning on Process Models: A Logic Programming Approach](https://arxiv.org/abs/1410.1776)
- 1994, article, "Formalizing architectural connection", [pdf](http://web.cs.wpi.edu/~cs562/s98/pdf/wright-icse16.pdf)
- [NOOL 2015 accepted papers - SPLASH 2015](https://2015.splashcon.org/track/nool2015#event-overview) ("New Object Oriented Languages")
    - "Classes Considered Harmful", [pdf](http://web.cecs.pdx.edu/~black/publications/ClassesHarmful.pdf)
    - "Ubiquitous Object Orientation to Foster the Advancement of Programming Languages", [pdf](http://www.cs.cmu.edu/~dkurilov/papers/nool15.pdf)
- NOOL 2016 articles
    - "Nomen: A Dynamically Typed OO Programming Language, Transpiled to Java", [pdf](http://www.it.uu.se/workshop/nool16/nool16-paper9.pdf)
        - "Nomen is an experimental, dynamically typed OO programming language which compiles to Java source code."
        - "Nomen is designed as a language for experimenting with IDE support generation using the Rascal language workbench."
    - "The essence of subclassing", [pdf](http://www.it.uu.se/workshop/nool16/nool16-paper5.pdf)
    - "Towards Automatic Decoration", [pdf](http://www.it.uu.se/workshop/nool16/nool16-paper2.pdf)
    - syntax
        - "Polite Programmers, Use Spaces in Identifiers When Needed", [pdf](http://www.it.uu.se/workshop/nool16/nool16-paper10.pdf)
- [WP:Comparison of functional programming languages](https://en.wikipedia.org/wiki/Comparison_of_functional_programming_languages)
- Designing APIs
    - [An API Ontology - Literate Programming](http://blog.steveklabnik.com/posts/2012-02-13-an-api-ontology)
- Tools
    - Golang
    - Ruby gem and bundler
    - Python pip
- Great Works in Programming Languages, Collected by Benjamin C. Pierce http://www.cis.upenn.edu/~bcpierce/courses/670Fall04/GreatWorksInPL.shtml
- to read? "theories of programming languages reynolds"
- 2017, book, "Principles of Programming Languages" https://www.cs.bgu.ac.il/~mira/ppl-book-full.pdf
- 2003, article, "Composing Programming Languages by Combining Action-Semantics Modules" http://www.brics.dk/RS/03/53/BRICS-RS-03-53.pdf
- 2001, position paper, "Composition Languages for Black-Box Components" http://scg.unibe.ch/archive/papers/Wuyt01c.pdf
- glue: Make better services. (deprecated) https://hackage.haskell.org/package/glue
- yet another music programming language https://github.com/alda-lang/alda
- linearscan: Linear scan register allocator, formally verified in Coq; 2004, master thesis, https://hackage.haskell.org/package/linearscan
- Lastik: A library for compiling programs in a variety of languages (Java, Scala, C#) https://hackage.haskell.org/package/Lastik
- risc386: Reduced instruction set i386 simulator https://hackage.haskell.org/package/risc386
- 2017, "Theorems for Free for Free", Wadler http://homepages.inf.ed.ac.uk/wadler/topics/blame.html
    - What is "polymorphic blame calculus"?
- https://idris.readthedocs.io/en/v1.3.0/faq/faq.html#what-are-the-differences-between-agda-and-idris
    - "Why does Idris use eager evaluation rather than lazy?"
        - "What is the representation of `thing` at run-time? Is it a bit pattern representing an integer, or is it a pointer to some code which will compute an integer? In Idris, we have decided that we would like to make this distinction precise [...]"
        - Idris has laziness, but you have to be explicit.
- https://en.wikipedia.org/wiki/Automatic_programming
- https://en.wikipedia.org/wiki/Program_synthesis
- https://www.cs.cmu.edu/~mleone/language-research.html
- "Confessions Of A Used Programming Language Salesman: Getting The Masses Hooked On Haskell", Erik Meijer, [pdf](https://pdfs.semanticscholar.org/233a/932b3e94f1f117655e4862995b32f33754be.pdf)
    - What are the key points?
- 1966, article, P. J. Landin, "The next 700 programming languages", [pdf](https://www.cs.cmu.edu/~crary/819-f09/Landin66.pdf)
    - https://en.wikipedia.org/wiki/Off-side_rule
- http://matt.might.net/articles/best-programming-languages/
- [EWD641: On the interplay between mathematics and programming](http://www.cs.utexas.edu/users/EWD/ewd06xx/EWD641.PDF)
- http://hackage.haskell.org/package/Workflow
- https://pchiusano.github.io/2017-01-20/why-not-haskell.html
- http://unisonweb.org/2015-05-07/about.html#post-start
- [facebook/duckling: Language, engine, and tooling for expressing, testing, and evaluating composable language rules on input strings.](https://github.com/facebook/duckling)
- [GaloisInc/crucible: Crucible is a library for symbolic simulation of imperative programs](https://github.com/GaloisInc/crucible)
- 2009, "Domain-Specific Languages for Composable Editor Plugins"
    - [2009 slides pdf](https://pdfs.semanticscholar.org/presentation/85d8/bc42122ff5175be1ebc3c7b91e4abff55d22.pdf)
    - [2010 article pdf](http://bora.uib.no/bitstream/handle/1956/9721/1-s2.0-S1571066110001179-main.pdf?sequence=1)
- why not PEG parsing
    - http://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2015/03/peg.html
- answer set programming
    - [News: Speed up solving complex problems: be lazy and only work crucial tasks - Aalto University](http://cs.aalto.fi/en/current/news/2018-07-18/)
        - 2018, article, "Exploiting Justifications for Lazy Grounding of Answer Set Programs", [pdf](https://www.ijcai.org/proceedings/2018/0240.pdf)
- [Idris as a Library - BAM Weblog](https://brianmckenna.org/blog/idris_library)
    - Idris as compiler backend
- Prolog ontology?
    - What is the relationship between Prolog, logic programming, ontology, and relational databases?
        - [What are ontology can do, but relational database can not? - Stack Overflow](https://stackoverflow.com/questions/29062541/what-are-ontology-can-do-but-relational-database-can-not)
            - [Ontologies and DB Schema: What's the Difference?](https://www.slideshare.net/UscholdM/ontologies-and-db-schema-whats-the-difference)
            - 2011, article, "Ontologies versus relational databases: Are they so different? A comparison", [pdf available](https://www.researchgate.net/publication/251332115_Ontologies_versus_relational_databases_Are_they_so_different_A_comparison)
            - 2010, article, "Mapping between Relational Databases and OWL Ontologies: an example", [pdf](https://www.lu.lv/materiali/apgads/raksti/756_pp_99-117.pdf)
    - [Salmon Run: Ontology Rules with Prolog](http://sujitpal.blogspot.com/2009/06/ontology-rules-with-prolog.html)
    - [SWI-Prolog for the (semantic) web](http://www.swi-prolog.org/web/)
- [Paul Chiusano: If Haskell is so great, why hasn't it taken over the world? And the curious case of Go.](https://pchiusano.github.io/2017-01-20/why-not-haskell.html)
    - Unison programming language
- Elixir has gradual static typing via Erlang Dialyzer.
    - [Typespecs and behaviours - Elixir](https://elixir-lang.org/getting-started/typespecs-and-behaviours.html)
    - [Understanding Elixir Types - via @codeship](https://blog.codeship.com/understanding-elixir-types/)
        - "Elixir functions are set up so that they can transparently be called across processes, heaps, or even machines in a cluster."
        - Can BEAM/Erlang/Elixir do live process migration?
    - The catch?
        - [Typed Elixir - Elixir Chat - Elixir Forum](https://elixirforum.com/t/typed-elixir/1388)
            - Is Dialyzer slow?
                - "My motivation for this is 95% of my bugs in Elixir/Erlang are due to using types wrong, like I may slightly change a tuple format somewhere but do not update it elsewhere and dialyzer does not catch it because the prior library state was in its cache that I then need to rebuild, in addition to dialyzer can take a long time to run."
- John Hughes, "Deriving combinator implementations", lecture 4, "Designing and using combinators" http://www.cse.chalmers.se/~rjmh/Combinators/DerivingCombinators/sld001.htm
- http://matt.might.net/articles/best-programming-languages/
- http://matt.might.net/articles/compiling-to-java/
- other programming languages
    - https://en.wikipedia.org/wiki/Curry_(programming_language)
    - http://fsl.cs.illinois.edu/images/5/5e/Cayenne.pdf
- [Extension programming language?](https://github.com/edom/work/blob/master/meta/doc/extension.md)
- https://en.wikipedia.org/wiki/Higher-order_abstract_syntax
- http://www.stephendiehl.com/posts/haskell_2017.html
- Haskell library: yaml vs HsYaml
    - https://twitter.com/hvrgnu/status/1004136566984503297
        - HsYaml is pure Haskell (doesn't use external libraries)
- [LCF key ideas](https://www.cl.cam.ac.uk/~jrh13/slides/manchester-12sep01/slides.pdf)
- closed source?
    - given SQL database, generate HTML user interface http://datanovata.com/
- http://libcello.org
- C HTTP server library?
    - https://kore.io
    - http://facil.io
- Possible user questions
    - How do I write software with this?
    - What are the important types?
- Don't format source code manually.
    - https://github.com/google/google-java-format
- related software
    - refactoring tools
        - https://github.com/RefactoringTools/HaRe
        - https://hackage.haskell.org/package/haskell-tools-refactor
    - parsing without symbol solving
        - Haskell and GHC extensions
            - http://hackage.haskell.org/package/haskell-src-exts
        - Haskell 98 only
            - https://hackage.haskell.org/package/haskell-src
        - Java
            - http://hackage.haskell.org/package/language-java
    - unknown
        - http://hackage.haskell.org/package/haskell-tools-ast
    - multi-database/cross-database query
        - http://www.unityjdbc.com/doc/multiple/multiplequery.php
        - https://www.red-gate.com/simple-talk/dotnet/net-tools/a-unified-approach-to-multi-database-query-templates/
    - similar systems
        - ERP libraries?
            - Meta is similar to Apache Ofbiz.
                - Some differences:
                    - To define entities, Meta uses Haskell, Ofbiz uses XML.
                    - Meta is written in Haskell, Ofbiz is written in Java.
                - https://cwiki.apache.org/confluence/display/OFBIZ/OFBiz+Tutorial+-+A+Beginners+Development+Guide
        - Web frameworks? Scaffolders?
            - Meta is similar to Laravel.
                - https://www.quora.com/Is-Laravel-a-good-framewok-really
            - Meta is similar to Ruby on Rails.
        - PhD theses
            - ["Programming Language Features for Web Application Development", Ezra Cooper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.422.5683&rep=rep1&type=pdf)
                - "Links" programming language
- For JDBC URL see
    - https://jdbc.postgresql.org/documentation/80/connect.html
- similar
    - https://medium.com/airbnb-engineering/react-native-at-airbnb-f95aa460be1c
- some requirement?
    - https://en.wikipedia.org/wiki/Multitenancy
- Name?
    - HUMPS Haskell Universal Meta Programming System ?
    - Hemps: Haskell Meta Programming System
    - EAG: Enterprise Application Generator
    - HAG: Haskell Application Generator
- https://en.wikipedia.org/wiki/Language-independent_specification
- http://referaat.cs.utwente.nl/conference/12/paper/7000/expressing-ontologies-using-a-functional-language.pdf
    - "there are some proposals for implementing subtyping [in Haskell] [11, 12]"
    - open ADT makes exhaustive case impossible
- sublanguages?
    - Ontology definition language
    - Data definition language
    - Web application description language
        - View description language
- software design
    - functional programming software design
        - designing combinators
            - Hughes 1995 doc [The design of a pretty-printing library](http://belle.sourceforge.net/doc/hughes95design.pdf)
- [LTU:progress on gradual typing](http://lambda-the-ultimate.org/node/5292)
- [WP:lambda-prolog](https://en.wikipedia.org/wiki/%CE%9BProlog)
- Should we use Haskell or TypeScript for this project? Both? Neither?
    - Killer features
        - IDE: TypeScript wins (VS Code).
        - custom infix operators: Haskell wins.
            - We can go even wilder with Agda, Coq, Idris, Lean, etc.
        - untagged unions: TypeScript wins.
        - software diversity: TypeScript wins.
            - TypeScript works with nodejs and npm. Boatloads of software.
            - I think TypeScript has more developers.
        - laziness
            - Haskell wins.
    - Both have
        - ADTs.

## Enabling metaprogramming

- [metaprogramming - Why is ML called Meta-Language? - Stack Overflow](https://stackoverflow.com/questions/50490883/why-is-ml-called-meta-language)
- related?
    - https://github.com/PostgREST/postgrest
    - http://rosecompiler.org/
- Metaprogramming
    - http://kaitai.io/
        - from description, generate parsers for binary data (as opposed to text data)
- Aspect-oriented programming is a restricted form of metaprogramming.
    - relationship between Aspect-Oriented Programming and Functional Programming
        - 2009, article, "What Does Aspect-Oriented Programming Mean for Functional Programmers?", [pdf](https://www.cs.ox.ac.uk/files/2282/wgp14-wang.pdf)
        - 2008, article, "On Feature Orientation and Functional Programming", [pdf](https://pdfs.semanticscholar.org/522e/b6c2ea910ed074a13fe21767c9fa070fb685.pdf)
        - 2016, article, "Realtime collaborative editor. Algebraic properties of the problem.", [html](http://blog.haskell-exists.com/yuras/posts/realtime-collaborative-editor.html)
            - see also Darcs patch theory
        - 2008, PhD thesis, "An Integrated System to Manage Crosscutting Concerns in Source Code", [pdf](http://wwwtmp.st.ewi.tudelft.nl/arie/phds/Marin.pdf)
        - 2003, article, "Language-independent aspect-oriented programming", [pdf available](http://www.tara.tcd.ie/handle/2262/32627)
- Java metaprogramming
    - Similar products
        - libraries
            - [INRIA Spoon](https://github.com/INRIA/spoon)
            - The `javax.lang.model` package of the Java standard library, but it does not model method bodies.
        - environments
            - [Eclipse Modeling Framework (EMF)](http://www.eclipse.org/modeling/emf/)
            - [JetBrains MPS (Meta Programming System)](https://www.jetbrains.com/mps/)
            - [Stratego/XT](http://strategoxt.org/)
            - TXL
            - [Eclipse Xtext](http://www.eclipse.org/Xtext/) and [Eclipse Xtend](http://www.eclipse.org/xtend/)
        - programming languages
            - Eclipse Xtend
        - parser generators
            - [WP:Compiler-compiler](https://en.wikipedia.org/wiki/Compiler-compiler)
            - [WP:Comparison of parser generators](https://en.wikipedia.org/wiki/Comparison_of_parser_generators)
            - [ANTLR (Another Tool for Language Recognition)](http://www.antlr.org/)
            - [JavaCC](https://javacc.org/)
            - YACC, Bison; with Lex, Flex
    - Related concepts
        - Model-driven development
        - Model-driven architecture

## Automatic (program) differentiation

- What is the relationship between incremental lambda-calculus and automatic differentiation of programs (or of algebraic data types)?

## Extending functions

Not only classes, but also functions, should be extensible.

"To extend the function \\( f : A \to B \\) to the function \\( f' : A' \to B' \\)" means:

- For every \\( x \in A \\), we have \\( f(x) = f'(x) \\).
- \\( A \subseteq A' \\).
- \\( B \subseteq B' \\).

A consequence:
Every occurrence of \\( f \\) can be replaced with \\( f' \\) while preserving the meaning of the containing expression.

## Low-code? Programming for the masses?

Limited programming?

What can we assume about the user's skill/knowledge/background?

- https://en.wikipedia.org/wiki/End-user_development
- https://en.wikipedia.org/wiki/Low-code_development_platforms

## Old content to be reorganized

- [Functional programming research]({% link functional_programming.md %})
    - Functional programming in the real world
        - Philip Wadler's list [Functional Programming in the Real World](http://homepages.inf.ed.ac.uk/wadler/realworld/)
- [Joint research with Abdullah]({% link abdullah.md %})
- [Programming]({% link programming.md %})

## Probably irrelevant to our goal

- [WP:Non-English-based programming languages](https://en.wikipedia.org/wiki/Non-English-based_programming_languages)
- Obscure things. Much marketing, little technical detail.
    - VPRI, Alan Kay et al., archived (stopped operating in 2018), computing for the masses?
        - "Improve 'powerful ideas education' for the world's children and to advance the state of systems research and personal computing"
            - https://harc.ycr.org/
                - https://www.ycr.org/
        - https://en.wikipedia.org/wiki/Viewpoints_Research_Institute
        - https://en.wikipedia.org/wiki/COLA_(software_architecture)
        - https://news.ycombinator.com/item?id=11686325
        - FONC = fundamentals of new computing
        - http://www.vpri.org/index.html
    - YCR
        - visual programming language
            - blocks language
                - https://harc.ycr.org/project/gp/
- Functional Payout Framework http://lambda-the-ultimate.org/node/3331
