---
title: Programming
permalink: /programming.html
date: 2017-06-22 00:28:00 +0700
---

- [Scheme currying]({% link scheme-curry.md %})
- Metaprogramming
    - [JetBrains MPS]({% link jbmps.md %})
    - [TXL: source transformation by example](http://txl.ca/index.html), what can it do?
    - http://www.moserware.com/2008/06/ometa-who-what-when-where-why.html
- [Comparing Objective Caml and Standard ML](http://adam.chlipala.net/mlcomp/), Adam Chlipala
    - https://people.mpi-sws.org/~rossberg/sml-vs-ocaml.html
- Programming languages don't work with each other very well.
    - What we need is not a new programming language, but a way for languages to work together?
- A program is
    - a sequence of instructions (procedural programming)
    - a lambda expression (functional programming)
- [compression-oriented programming](https://mollyrocket.com/casey/stream_0019.html)
- Lisp family/dialects
    - Scheme
    - [PicoLisp](https://picolisp.com/wiki/?home)
- Undigested
    - https://hakonrossebo.github.io/functional-programming-babelfish/
    - https://github.com/rainbyte/haskell-ide-chart
    - http://docs.mamedev.org/techspecs/luaengine.html
- The source code is a textual representation of the abstract syntax tree (AST).
- The source code is unimportant cosmetics.
We should care about the ASTs and transformations between ASTs.
We should read, write, and edit ASTs directly, not text.
The editor should not parse text into AST; it should generate text from AST.
- Language-oriented programming? Data-first programming?
    - Data is the most important thing.
        - Because data is harder to back up than code?
    - Program translates the shape of data.
    - Describe what you want, and write code that follows that description
    to call the lower-level implementation details.
- Programming language design
    - Which should we start with?
        - Semantics (high-level, top-down language design)
        - Machine code (low-level, bottom-up language design)
    - Every enhancement reduces cognitive load.
    - Example of bottom-up language design and how each level reduces cognitive load:
        - Begin with machine code.
        - Provide mnemonics for instructions.
        - Provide the illusion of infinite custom-named registers and orthogonal operands.
        - Provide macros subroutines as extensible instructions.
        - Provide the illusion of infinite custom-named registers and orthogonal operands.
        - Provide macros and subroutines as extensible instructions.
        - Provide named locations.
        - Provide the illusion of infinite memory.
        - Abstract away processor registers.
        - Abstract away pointers.
        - Expression.
        - Infix expression syntax.
        - First-class functions.
        - The program itself is a procedural program that tells the interpreter what code to generate.
        - End up with something like Randall Hyde's High Level Assembly?
- Improve an existing language instead of starting new?
- What is this? http://reasonablypolymorphic.com/blog/elm-is-wrong
- Interesting languages?
    - Rebol? Rebol metaprogramming? https://en.m.wikipedia.org/wiki/REBOL
    - Carp lisp dialect?
- Scheme vs Lisp:
    - A Lisp implementation doesn't have to have proper tail calls.
    - A Scheme implementation must have proper tail calls.
- Type systems
    - Types help computers help us.
        - Types prevent some mistakes.
        - Types are part of documentation.
            - Types help us write an IDE.
