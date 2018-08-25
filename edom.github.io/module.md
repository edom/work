---
title: Designing module systems
permalink: /module.html
date: 2018-08-22 00:12 +0700
---

What is a module?
Or, should we be asking these questions instead?

- How do we decompose a program? (I think David Parnas has answered this.)
- Why do we need modules?
- How do we organize programs?

Modules reduce complexity by partitioning and independence.
See [Three Universal methods of reducing complexity](http://www.computing.dcu.ie/~renaat/ca2/ca214/ca214vii.html)
from the course [CA214 Systems Analysis and Design Page](http://www.computing.dcu.ie/~renaat/ca2/ca214over.html).

What do others say?

- [Wikipedia](https://en.wikipedia.org/wiki/Module)
- According to [Wiktionary](https://en.wiktionary.org/wiki/module):
    - The word "module" is from Latin "modulus" that means "a small measure".
    - A module is "a self-contained component of a system, often interchangeable, which has a well-defined interface to the other components".
- [dictionary.com](https://www.dictionary.com/browse/module)

What do we infer?

- We can develop different modules at the same time.

## Philosophical investigation

- What are the properties of a module?
- What are its relationships with other things?
    - interchangeability
- A module groups things.
    - Is this essential or accidental?
    Is it made for grouping?
    Is grouping only a side-effect?
- What can we do with modules?
    - We can combine modules.
    - We can shadow modules.
    - We can link modules.
    - We can embed/inline modules.
- A module is an incomplete/dependent piece of functionality/code.
    - A module may have unresolved symbols?
- A module is a decomposition of a program?
- Module is about reusability?
- A program is a module and a starting point.
- A module specifies a contract.
A module can be swapped with another module that satisfies the same contract without changing the correctness of the program.
- A module is a bunch of imports and exports?
- A module is smallest unit of reuse? Isn't that function?
- A module is smallest unit of compilation? Isn't that function?

## Modules are for humans

Computers don't need modules.
All it needs is a sequence of machine codes.

A program is a lambda expression.
A big program is a big lambda expression.
Given enough memory, a computer can handle arbitrarily big lambda expressions.

Human uses modules for organizing things.
Human uses modules to make machines separate compilation, speed up recompilation, and recompile a part of the program.

- Does a module have to coincide with a compilation unit?
- Basic module functions?
    - How does a code describe its dependencies?
    - How does the machine disambiguate names?
    - Functions should be versioned. Not module. Not package.
    Version describes semantics.

Module is second-order logic programming?
Note below, that the same `Plus` is used as both a variable and a predicate.

```
export(module_name, type, name, value).

export(prelude, int, plus, Plus) |- export(my_module, int, three, Plus(1, 2)).
```

- The smallest unit for this discussion is a machine instruction.
- A subroutine is a collection of instructions.
- A library is a collection of subroutines.
- A program is a collection of libraries and an entry point.
- History
    - The initial motivation was to reuse.
        - Reduce development cost.
            - Humans have always been looking for easier ways to live.
            This "laziness" (the ability to get bored repeating something) is the source of all human technology.
    - The next motivation was to reduce disk and memory usage.
- The essence of programming-in-the-large is Don't Repeat Yourself?

## Comparing existing module systems

How do programming languages deal with modules?

- [dhall modules · Issue #182 · dhall-lang/dhall-lang](https://github.com/dhall-lang/dhall-lang/issues/182)
- [Futhark](https://futhark-lang.org/blog/2017-01-25-futhark-module-system.html)
- [Elixir](https://elixir-lang.org/getting-started/modules-and-functions.html)
    - [Module – Elixir v1.7.2](https://hexdocs.pm/elixir/Module.html)
- Racket
    - 2011, article, "Languages as Libraries", [pdf](http://www.cs.utah.edu/plt/publications/pldi11-tscff.pdf)
- Scheme R7RS, Common Lisp, Clojure
- Java, Scala, Kotlin, Go, C, C++
    - C ABI
        - A module is an ELF shared object file (SO file).
- Pascal, Ada, Oberon, Algol, Fortran
- JavaScript, TypeScript, ECMAScript
- Standard ML, Caml, OCaml, MLTON, SML/NJ, F#
    - 2000, "A modular module system", [pdf](https://hal.inria.fr/hal-01499946/document)
- Haskell has underpowered module system.
- book, "Advanced topics in types and programming languages", part IV, programming in the large, [pdf](http://camlunity.ru/swap/Functional%20Programming/Type%20Systems/Pierce/Advanced%20Topics%20in%20Types%20and%20Programming%20Languages.pdf)
    - book, "Types and programming languages", [pdf](https://www.asc.ohio-state.edu/pollard.4/type/books/pierce-tpl.pdf)
- [WP:Modular programming](https://en.wikipedia.org/wiki/Modular_programming)

## Key idea: Module = Dictionary -> Dictionary

Assume a dependently-typed language.

Recall some terminologies:

- A record is a tuple whose components are named.
- A dictionary is also known as key-value map or look-up table.

Then a module is a lambda abstraction that takes a record and gives a record.

A module is a lambda abstraction.

This idea is similar to Nix and JavaScript modules.

`type Module = Map Name Decl -> Map Name Decl`

A module translates into a lambda-calculus expression.
An import translates to an entry in the input dictionary.
An export translates to an entry in the output dictionary.
Example:

```
module {
    import add mul Int32;
    export f g T;
    f = add;
    g = mul;
    T = Int32;
};

-- The expression above translates to:

\ {add; mul; Int32; ...} -> {
    f = add;
    g = mul;
    T = Int32;
};
```

- Key ideas of that example:
    - Dictionary pattern matching simulates row polymorphism.
    - `{a;b;c;}` is shorthand for `{a:a; b:b; c:c;}`.
    - [Notes on Elixir: Pattern-Matching Maps · Rob Phoenix](https://blog.robphoenix.com/elixir/notes-on-elixir-pattern-matching-maps/)

What we are doing here is also known as "blurring the phase distinction".
See ["Modules versus Higher-Order Functions" in Futhark blog post](https://futhark-lang.org/blog/2017-01-25-futhark-module-system.html#modules-versus-higher-order-functions):
"A module can be viewed as nothing but a record containing types and values."

A problem: compilation may fail to terminate.
No big deal.
Set a time-out.

## what

- Hypothesis: Lazy evaluation solves the partial query problem elegantly.
- What are some cool ideas?
    - Dhall can import from IPFS.
        - http://www.haskellforall.com/2016/12/dhall-non-turing-complete-configuration.html
    - Elixir can pattern-match maps (dictionaries).
- What is a module in an untyped functional programming language such as Tulip?
- Finding a programming language for programming in the large
    - Ecosystem, libraries, tools, and communities.
    - The most important thing in programming in the large is name management.
    Namespaces.
        - C has two namespaces: type namespace and value namespace.
        - Haskell has two namespaces: type namespace and value namespace.
        - Java has better namespacing than C.
        - Enable the same name to be used in different context, so that you can write `get_name employee` and `get_name company` instead of `employee_get_name employee` or `company_get_name company`.
            - Ad-hoc polymorphism.
    - Which one has the biggest community?
    - Which one has a decent IDE?
    - Which community puts their money on where their mouth is?
    - Comparing type systems
        - [The Typed Racket Guide](https://docs.racket-lang.org/ts-guide/)
        - F#
        - SML
        - Caml
        - OCaml
        - Idris, Agda
        - Coq, Lean
        - Haskell
        - 2004, chapter, "Type systems", Luca Cardelli, [pdf](http://lucacardelli.name/Papers/TypeSystems.pdf)
            - from https://www.artima.com/forums/flat.jsp?forum=106&thread=185420
            - 2005, book, "Advanced topics in types and programming languages", Benjamin C. Pierce (editor)
                - Part IV, "Types for Programming in the Large"
            - 2002, book, "Types and programming languages", Benjamin C. Pierce
        - Java, Kotlin, Scala
        - Things that annoy me
            - ML, SML, Caml, OCaml: `'a tf` is somewhat annoying. It should have been `tf a`.
                - F# uses `tf<'a>`.
                - Haskell uses `Tf a`.
            - Would you rather type `'a list` (F#) or deal with an inadequate record/module system (Haskell)?
            - Haskell doesn't have `instance Read (->)` and `instance Show (->)`.
                - Haskell expressions are not first-class citizen in the language.
                    - Unlike Lisp/Scheme.
                - Encumbers metaprogramming.
        - OCaml labels and polymorphic variants?
            - http://caml.inria.fr/pub/docs/manual-ocaml-400/manual006.html
            - OCaml labels are somewhat similar to Scheme keyword arguments.
        - F# quotations is important for metaprogramming.
        - F# doesn't do ad-hoc polymorphism well?
            - [Ad-hoc Polymorphism in F# (how to survive without Type Classes) - Without the loop](https://withouttheloop.com/articles/2014-10-21-fsharp-adhoc-polymorphism/)
            - [Higher-kinded Polymorphism: What is it, why you want it · David Raab](https://sidburn.github.io/blog/2016/03/24/higher-kinded-polymorphism)
        - https://cstheory.stackexchange.com/questions/40705/why-did-caml-become-ocaml-or-why-use-objects-in-f
        - ML begat Caml. Caml begat Caml Light? Caml Light begat OCaml?
        - [How does F# compare to OCaml, in regard to major syntactic differences, paradigm shifts, and interoperability with Windows? What about its numeric capabilities? - Quora](https://www.quora.com/How-does-F-compare-to-OCaml-in-regard-to-major-syntactic-differences-paradigm-shifts-and-interoperability-with-Windows-What-about-its-numeric-capabilities)
            - Jon Harrop claims. More sources needed. Take it with a grain of salt.
                - "OCaml has an integrated full-blown macro system in the form of Camlp4 whereas F# does not have macros and, in fact, has been deliberately closed off in order to discourage people from creating products that compete with Visual Studio."
                    - "deliberately closed off [...]" is a bold claim.
    - [Are all languages basically the same? - Software Engineering Stack Exchange](https://softwareengineering.stackexchange.com/questions/155239/are-all-languages-basically-the-same/155243)
- package/dependency management tools
    - Java: Maven, Gradle
    - OCaml: OPAM
    - Haskell: Cabal, Stack
    - F#: Paket? NuGet?
    - C/C++: conan? chocolate? vcpkg?
- Formally adding modules to lambda calculus
    - What is module calculus?
        - 2017 article "Modules, Abstraction, and Parametric Polymorphism" [pdf](https://www.cs.cmu.edu/~crary/papers/2017/mapp.pdf)
        - 2003 article "A Type System for Higher-Order Modules" [pdf](http://www.cs.cmu.edu/~rwh/papers/thoms/tr2.pdf)
        - 2001 article "A Calculus of Module Systems" [pdf available](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.22.5407)
        - 2012 course notes "Types for Module Systems" [pdf](http://www.ccs.neu.edu/home/amal/course/7480-s12/modules-notes.pdf)
        from [CS7480 Type Systems (Spring 2012)](http://www.ccs.neu.edu/home/amal/course/7480-s12/)
- Ignored undocumented code sketches
    - [jordanlewis/simple-module-system: Adding modules to a polymorphic lambda calculus](https://github.com/jordanlewis/simple-module-system), code in SML/NJ.
