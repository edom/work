---
title: Extending Haskell
permalink: /exhask.html
date: 2018-07-22 02:45 +0700
---

- TOC
{:toc}

## Open ADTs (algebraic data types)

- "Closed" means "defined in one place".
- Open ADTs don't mix with exhaustive case analysis (function totality).
    - https://stackoverflow.com/questions/870919/why-are-haskell-algebraic-data-types-closed
    - But what if functions are "open" too?
        - https://www.andres-loeh.de/OpenDatatypes.pdf
- If `f : a -> b`, then the compiler should infer `lift f : (Monad m) => m a -> m b`.

## Can we extend Haskell to "auto-fmap"?

- Possibilities:
    - Add rewrite rules so that the compiler "recovers" from some type "errors".
    - Extend the syntax and semantics of function application.
- Related
    - 1989, article, Wadler, "Theorems for free!"
    - The Haskell Djinn can, given a type T, infer/construct a term having type T.
- Recovering from some type errors
    - Idea
        - Extend Haskell with "implicit injections".
        - The compiler should try in-scope injections automatically when there is a typing error, before quitting with a type error.
            - Isn't this similar to Scala implicits and implicit conversion?
                - I forgot who, but I think somebody on the Internet said that Scala implicits are a way for the compiler to recover from type errors.
        - Can we do this on GHC?
            - https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeChecker
                - GHC typechecker works on Haskell before it's transformed to Core.
            - Write a plugin for GHC?
                - Can a GHC modify the syntax tree on type error?
            - Use GHC as library?
            - We can't use GHC rewrite rules because they are only applied when optimization is enabled.
    - Define the concept of "expected type".
    - Let `e` be an expression.
    - Let `f : a -> b`.
    - Let `m` be an instance of Monad.
    - If `e` has type `a`, but the compiler expects `e` to have type `m a`, then the compiler shall rewrite `e` to `return e`.
    - If `e` has type `m a`, then the compiler rewrites `f e` to `map f e`.
- If `x` is a Monad, then these are two *different* things: `x : a` and `return x`, but they are related, in the sense that they are equivalent, in the sense that one is trivially computable/derivable from the other.
- Can Strathclyde Haskell Enhancement (SHE) do this?
    - It has idiom brackets.
    It translates `(| f a1 ... an |)` to `pure f <*> a1 <*> ... <*> an`.
        - https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/idiom.html
    - Enhancement to SHE https://github.com/bezirg/she
        - http://blog.bezirg.net/posts/2013-08-03-enhancement-to-the-strathclyde-haskell-enhancement.html
- https://en.wikipedia.org/wiki/Bidirectional_transformation
    - https://www.cis.upenn.edu/~bcpierce/papers/lenses-etapsslides.pdf

## Auto-lifting (and therefore sequencing) of function application involving Monad instances

- The standard rule is:
    - If `x : a` and `f : a -> b`, then `f x : b`.
- Suppose that `m` has a Monad instance.
    - If `x : m a` and `f : a -> b`, then should the compiler silently translate `f x` to `x >>= return . f`?
        - Isn't it the only desirable way of putting together `f` and `x`?
            - Monad class requires that `x >>= return . f` be equivalent to `fmap f x`.
                - So there is really only one way to do it, isn't it?
            - Examples of non-desirable ways: `unsafeCoerce`, `undefined`.
    - Should the compiler also appropriately translate `f x` for all these combinations?
        - Possibilities for the type of `x`:
            - `a`
            - `m a`
        - Possibilities for the type of `f`:
            - `a -> b`
            - `a -> m b`
            - `m (a -> b)`
            - `m a -> m b`
            - `m a -> b`
- At first glance it seems convenient, but what are the consequences?
    - Some I can think of
        - Confusing error message
            - Suppose:
                - The programmer makes a typing mistake.
                - The compiler infers the wrong type.
                - The compiler performs translation based on the wrongly inferred type.
                - The compiler produces a confusing error message.

## Equirecursive types?

Haskell has isorecursive types.
Can we make it use equirecursive types?

- Can we make it automatically insert roll-unroll/fold-unfold/In-out?
- How do we compose monads seamlessly?
    - Isorecursive types?
    - True sum types (untagged unions)?
- "System F-omega with Equirecursive Types for Datatype-Generic Programming"?
