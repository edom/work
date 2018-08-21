---
title: Designing module systems
permalink: /module.html
date: 2018-08-22 00:12 +0700
---

- A program is a big lambda expression. Why do we need modules?
- How do we organize programs?
    - The smallest unit for this discussion is a machine instruction.
    - A subroutine is a collection of instructions.
    - A library is a collection of subroutines.
    - A program is a collection of libraries and an entry point.
    - History
        - The initial motivation was to reuse.
            - Reduce development cost.
        - The next motivation was to reduce disk and memory usage.
    - The essence of programming-in-the-large is Don't Repeat Yourself?
- What is a module?
    - "What is a module?" = "How do we decompose a program?"
    - What do others say?
        - [Wikipedia](https://en.wikipedia.org/wiki/Module)
        - [Wiktionary](https://en.wiktionary.org/wiki/module)
        - [dictionary.com](https://www.dictionary.com/browse/module)
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
- Module is second-order logic programming?
Note below, that the same `Plus` is used as both a variable and a predicate.

```
export(module_name, type, name, value).

export(prelude, int, plus, Plus) |- export(my_module, int, three, Plus(1, 2)).
```

- Who uses module?
Human and computer.
    - Human uses modules for organizing things.
    - Computer uses modules to separate compilation, to speed up recompilation, to enable partial recompilation.
- Does a module have to coincide with a compilation unit?
- How do other languages deal with modules?
    - [dhall modules · Issue #182 · dhall-lang/dhall-lang](https://github.com/dhall-lang/dhall-lang/issues/182)
    - [Futhark](https://futhark-lang.org/blog/2017-01-25-futhark-module-system.html)
    - [Elixir](https://elixir-lang.org/getting-started/modules-and-functions.html)
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
- Basic module functions?
    - How does a code describe its dependencies?
    - How does the machine disambiguate names?
    - Functions should be versioned. Not module. Not package.
    Version describes semantics.
- Key idea: A module is just a lambda abstraction that returns a record (tuple whose components are named), in a dependently-typed language.
    - Module = lambda-calculus + dictionary
    - This idea is similar to Nix and JavaScript modules.
    - `type Module = Map Name Decl -> Map Name Decl`
    - A module translates into a lambda-calculus expression.
    - An import translates to an entry in the input kvm.
        - kvm = key-value map, look-up table, dictionary
    - An export translates to an entry in the output kvm.
    - Example:

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
- Hypothesis: Lazy evaluation solves the partial query problem elegantly.
- What are some cool ideas?
    - Dhall can import from IPFS.
        - http://www.haskellforall.com/2016/12/dhall-non-turing-complete-configuration.html
    - Elixir can pattern-match maps (dictionaries).
- What is a module in an untyped functional programming language such as Tulip?
