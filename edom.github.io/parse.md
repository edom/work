---
title: Parsing
permalink: /parse.html
date: 2018-08-25 00:50 +0700
mathjax: yes
---

I expect the computer to infer a parser and a pretty-printer from the same grammar.
Parser generators only give half of what I want.

- TOC
{:toc}

## What is parsing?

Parsing is also called "syntax analysis" (analysis = breakdown, syntax = put together).

Parsing is the act of modifying the *state* of the parser.
This is the operational view.

Parsing is converting a sequence to a tree.
This is the data view.

Trees are graphs. Why stop at trees? Why not graphs?

- [WP:Graph rewriting](https://en.wikipedia.org/wiki/Graph_rewriting)
- 2015 slides "Graph grammars" [pdf](http://www.its.caltech.edu/~matilde/GraphGrammarsLing.pdf)
- 1993 article "A Graph Parsing Algorithm and Implementation" [pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.612.9698&rep=rep1&type=pdf)
- 1990 article "A Graph Parsing Algorithm" [paywall](https://dl.acm.org/citation.cfm?id=859753)

What is the difference between syntax and grammar?

### Lexical analysis

We lex (perform lexical analysis / tokenization) to clean up the grammar (no need to mention whitespaces in the grammar).
For example:

- With lexing:

```
exp ::= exp PLUS exp
```

- Without lexing:

```
white ::= ...
exp ::= exp white "+" white exp
```

"Strictly speaking, tokenization may be handled by the parser.
The reason why we tend to bother with tokenising in practice is that it makes the parser simpler,
and decouples it from the character encoding used for the source code."
([Wikibooks:Compiler construction](https://en.wikibooks.org/wiki/Compiler_Construction/Lexical_analysis))

## What is the inverse of parsing?

Unparsing?

Parsing is the treeization (delinearization, deserialization) of a line.
Unparsing is the linearization (serialization) of a tree.

Parsing is String -> Maybe Tree.
Unparsing is Tree -> String.

Can we make parsing truly one-to-one?
String -> Tree.
CST = AST.
Very rigid syntax.
Forbid whitespace freedom.

Another possibility: Inverse of parsing is anti-parsing (generation)?
From grammar, generate all possible strings and their syntax trees.

Inverse of analytical grammar is generative grammar?

- https://en.wikipedia.org/wiki/Generative_grammar
- https://en.wikipedia.org/wiki/Formal_grammar#Analytic_grammars

Parser is syntax analyzer.
Analysis is the opposite of synthesis?
What is syntax synthesizer?

Inverse of parsing is pretty-printing?

If matching is analogous to subtraction, then what is analogous to multiplication?
Generation?

- algebra of pretty-printing
    - 1995, Hughes, "The design of a pretty-printing library"
    - 1998, Wadler, "A prettier printer"
    - Hughes, Peyton-Jones, et al., http://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint-HughesPJ.html
- [Efficient simple pretty printing combinators](https://www.cs.kent.ac.uk/people/staff/oc/pretty.html)

## How should parsing be done?

From grammar description, the machine should generate both a parser and a pretty-printer.

Given grammar, generate both parser and unparser/pretty-printer.
- http://www.semdesigns.com/Products/DMS/DMSPrettyPrinters.html?Home=DMSToolkit
- https://hackage.haskell.org/package/invertible-syntax-0.2.1/src/Example.lhs
- https://hackage.haskell.org/package/invertible-syntax
- [Tillmann Rendel and Klaus Ostermann. "Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing". In Proc. of Haskell Symposium, 2010.](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)
- http://jssst.or.jp/files/user/taikai/2016/PPL/ppl1-1.pdf
- [LTU: Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing](http://lambda-the-ultimate.org/node/4191)
- [Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)

Some parsing techniques:

- recursive descent parser (writing a parser manually)
- parser generators: Happy (Haskell), Bison (with Yacc)
- parser combinators: Parsec (Haskell)
- PEG (parsing expression grammar)
- Brzozowski quotient
- binary-parser description languages: ASN.1, Google Protobuf, Apache Thrift, Apache Avro
- invertible parsing?
- https://en.wikipedia.org/wiki/Chart_parser

## Incremental/online parsing

Incremental parsing is parsing as input becomes available (without waiting for the whole input to become available).

- Type-directed automatic incrementalization
    - http://www.cs.cmu.edu/~joshuad/papers/incr/
- https://en.wikipedia.org/wiki/Incremental_computing
    - https://inc-lc.github.io/
- https://hackage.haskell.org/package/incremental-parser
- [incremental/online parsing](https://yi-editor.github.io/posts/2014-09-04-incremental-parsing/)

## Parsing with Brzozowski quotients

Brzozowski quotient is like quotient in integer division, but for strings.
(Formal definition?)

Why is Brzozowski quotient called Brzozowski derivative?

- [Quotient of a formal language](https://en.wikipedia.org/wiki/Quotient_of_a_formal_language)
- [Brzozowski derivative](https://en.wikipedia.org/wiki/Brzozowski_derivative)
    - [Janusz Brzozowski et al. on arxiv](https://arxiv.org/find/cs/1/au:+Brzozowski_J/0/1/0/all/0/1)

The *multiplication* of two strings \\( x \\) and \\( y \\) is the concatenation \\( x \cdot y = x y \\).

Multiplication is associative: \\( (xy)z = x(yz) \\).

The *inverse* of a string \\( x \\) is written \\( x^{-1} \\).
It's hypothetical.
It's pure symbolic manipulation.
Don't imagine what it looks like.
Do care about its properties:

- We define \\( x^{-1} x = \epsilon \\).
- We define \\( x x^{-1} = \epsilon \\).
- We define \\( (x y)^{-1} = x^{-1} y^{-1} \\).

The *left division* of a string \\( x \\) by divisor \\( y \\) is \\( y^{-1} x \\).

The *right division* of a string \\( x \\) by divisor \\( y \\) is \\( x y^{-1} \\).

How do we define quotient and remainder?

Perhaps Brzozowski's paper describes why it's called a derivative?

The multiplication of two languages \\( A \\) and \\( B \\) is the Cartesian product \\( AB = \\{ ab ~\|~ a \in A, b \in B \\} \\).

- Differentiating Parsers
Automatic derivation of incremental parser from a grammar or a non-incremental parser?
Like automatic differentiation but generalized to any program?
http://lambda-the-ultimate.org/node/3704
- http://matt.might.net/articles/implementation-of-regular-expression-matching-in-scheme-with-derivatives/
- http://okmij.org/ftp/continuations/differentiating-parsers.html
- Parsing with derivatives?
    - https://hackage.haskell.org/package/derp
    - https://arxiv.org/abs/1010.5023
    - http://matt.might.net/articles/parsing-with-derivatives/ "Yacc is dead"
- Brzozowski quotients.
    - [Yacc is dead](https://arxiv.org/abs/1010.5023)
    - "Parsing with derivatives"
- 2017, ["A Typed, Algebraic Approach to Parsing"](https://www.cl.cam.ac.uk/~nk480/parsing.pdf)
    - "[...] we extend the notion of Brzozowski derivative from regular expressions to the typed context-free expressions."

## History of parsing

- [Parsing: a timeline -- V3.0](https://jeffreykegler.github.io/personal/timeline_v3): 2012 article about a history of parsing.
    - [Parsing: a timeline. Hopefully this puts "Parsing is a solved problem" to rest. : ProgrammingLanguages](https://www.reddit.com/r/ProgrammingLanguages/comments/8cz97n/parsing_a_timeline_hopefully_this_puts_parsing_is/)
    - [Why is parsing considered solved?](http://jeffreykegler.github.io/Ocean-of-Awareness-blog/individual/2018/05/knuth_1965.html)

## What

- Parsing Expression Grammar (PEG)
    - https://github.com/harc/ohm/
        - https://ohmlang.github.io/
            - https://harc.ycr.org/project/ohm/
    - Packrat
- 2015, [Nez: practical open grammar language](https://arxiv.org/abs/1511.08307)
- Earley parser
    - https://en.wikipedia.org/wiki/Earley_parser
    - https://hackage.haskell.org/package/Earley
- https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#parsing--pretty-printing
    - https://hackage.haskell.org/package/trifecta
    - https://hackage.haskell.org/package/parsers
