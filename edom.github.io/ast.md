---
title: Solving the AST decoration problem
date: 2018-07-30 17:48 +0700
permalink: /ast.html
---

- common problem in parsing: how to decorate AST in a functional language
    - 2013, article, [The AST typing problem](http://blog.ezyang.com/2013/05/the-ast-typing-problem/), Edward Z. Yang
        - 2010, discussion, "The AST typing problem", [LTU](http://lambda-the-ultimate.org/node/4170)
    - Embracing the Any type
        - [JavaParser](http://javaparser.org/) added an `Object data` field to the AST Node base class, but they changed their minds.
            - https://github.com/javaparser/javaparser/issues/456
            - https://github.com/javaparser/javaparser/pull/472
    - Structural typing
        - TypeScript (JavaScript) is ideal for AST decoration problem?
        You just add a key to the AST node object you want to decorate.
            - Any language with structural typing is ideal?

            ```typescript
            interface Node {
            }

            interface Node_parse extends Node {
            }

            interface Node_desugar extends Node_parse {
            }
            ```

    - Loosely-typed Haskell?
        - This is basically the JavaScript way.

        ```haskell
        type Key = String
        data Value
            = VNum Double
            | VStr String
            | VObj Object
        type Object = [(Key, Value)]
        type Node = Object
        ```

    - 2017, article, "Trees that grow", https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf
        - "The compiler writer is then faced with two unpalatable choices.
        She can define a new data type representing the output decorated tree, at the cost of much duplication.
        Or she can write a single data type with all the necessary fields and constructors, at the cost of having many unused fields and constructors at different stages of compilation."
        - However, we can fake extensible ADTs with pattern synonyms, with Fix, and without type families.
            - Similar endeavors
                - https://wiki.haskell.org/Type_composition
            - Haskell doesn't beta-reduce types.
            - This is an example code:

            ```haskell
            data Exp_ name exp
                = Var name
                | Add exp exp
                | ...

            data Locd a
                = MkLocd Loc a

            data Typed t a = MkTyped t a

            newtype Compose f g a = MkCompose { unCompose :: f (g a) }

            type PsExp name = Fix (Compose Locd (Exp_ name))
            type TcExp name = Fix (Compose Locd (Compose Typed (Exp_ name)))

            -- To ameliorate the verbosity:

            class Exp name exp where
                var :: name -> exp
                add :: exp -> exp -> exp
                ...

            instance Exp (PsExp name) where ...
            instance Exp (TcExp name) where ...
            ```

    - What if GHC can "inline" data types at compile time?
    What if GHC can "inline" A and B in `data A = MA Int; data B = MB Int String; data E = EA A | EB B;`,
    producing `data E = EA Int | EB Int String`?
    Implementing this with Haskell 98 types should be straightforward.
    - related
            - "Data types a la carte"
                - http://hackage.haskell.org/package/compdata
            - Haskell type composition
            - https://wiki.haskell.org/Extensible_datatypes
