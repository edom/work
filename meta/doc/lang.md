Make a programming language for writing metaprograms.

- Requirements:
    - Clear semantics.
    - Translates to Haskell.
    - equational substitutability
    - first-class modules (Scheme R7RS like?)
        - combine two modules
        - renaming reexport
        - reexport
    - first-class classes
    - first-class instances
    - first-class types? dependent typing?
    - Design pattern generator?
        - generate ADT constructors and deconstructors
        - generate record getters and setters
        - generate untagged union from several ADTs
    - `:` instead of `::` and `::` instead of `:`
    - some supercompilation (compile-time partial evaluation)
    - consistent definition syntax
    - redefinable function application?
        - [not possible in Haskell](https://mail.haskell.org/pipermail/haskell-cafe/2007-May/026314.html)
    - overloading?

```
-- A module translates to a Haskell data type.
Module.Name = module {
    import all from Prelude;

    Maybe a = data {
        Nothing : Maybe a;
        Just : a -> Maybe a;
    } deriving Read Show Eq;

    Either a b = data {
        Left a;
        Right b;
    } deriving Read Show Eq;

    -- A class translates to a Haskell data type.
    Functor = \ (f : * -> *) -> class {
        fmap : (a -> b) -> f a -> f b;
    };

    Functor f = class {
        fmap : (a -> b) -> f a -> f b;
    };

    -- Parameterized module.
    MyMod a = module { };
    MyModEqu = \ a -> module { };

    -- Named instance of anonymous class. Equational substitutability.
    instance (class { fmap : (a -> b) -> f a -> f b; }) Maybe {
        fmap f (Just x) = Just (f x);
        fmap _ x = x;
    };

    -- A named instance translates to a Haskell value.
    i = instance Functor Maybe {
        fmap f (Just x) = Just (f x);
        fmap _ x = x;
    };

    -- Default instance. How does it interact with import?
    default instance Functor Maybe {
    };

    -- verbose syntax?
    default instance of Functor for Maybe is {
    };

    -- Define in let.
    -- Pattern.
    let
        Has = Just;
        None = Nothing;
    in {
        f : Maybe Int -> Int;
        f (Has x) = x;
        f None = 0;
    };

    zero : Int;
    zero = 0;

    ones : [Int];
    ones = 1 :: ones;

    add1 : Int -> Int;
    add1 = \ x -> x;

    -- Inline type signatures.
    add = \ (x y : Int) ->
        let import { +; } from Prelude;
            z = 1;
        in x + y : Int;
    infixl 4 `add2`;

    example = (let x = 1; in x) + (let x = 2; in x);

    id {a : *} (x : a) = x;
    id = \ {a} (x:a) -> x;

    and : Bool -> Bool -> Bool;
    and False _ = False;

    or : Bool -> Bool -> Bool;
    or x y = case x of {
        True -> True;
        False -> y;
    };

    -- how about context in type signatures?
};

-- My.Mod
My = module {
    Mod = module {
    }
}

-- Redefine syntax?
<application> a b = syntax {
};
```

- compiling with continuations
    - Why use CPS (continuation passing style) as intermediate form?
        - http://matt.might.net/articles/cps-conversion/
        - https://www.microsoft.com/en-us/research/publication/compiling-with-continuations-continued/
        - https://news.ycombinator.com/item?id=7150095
    - 2003, retrospective: the essence of compiling with continuations https://users.soe.ucsc.edu/~cormac/papers/best-pldi.pdf
        - https://en.wikipedia.org/wiki/A-normal_form
- relational programming (pure logic programming?)
    - miniKanren
        - Byrd PhD thesis https://scholarworks.iu.edu/dspace/bitstream/handle/2022/8777/Byrd_indiana_0093A_10344.pdf
            - mentions other programming languages: Prolog, Mercury, Curry
- Haskell has isorecursive types.
    - Can we make it use equirecursive types?
    - Can we make it automatically insert roll-unroll/fold-unfold/In-out?
    - How do we compose monads seamlessly?
        - Isorecursive types?
        - True sum types (untagged unions)?
    - 2016, System F-omega with Equirecursive Types for Datatype-Generic Programming http://ps.informatik.uni-tuebingen.de/research/functors/equirecursion-fomega-popl16.pdf
- John Hughes, "Deriving combinator implementations", lecture 4, "Designing and using combinators" http://www.cse.chalmers.se/~rjmh/Combinators/DerivingCombinators/sld001.htm
- http://matt.might.net/articles/best-programming-languages/
- other programming languages
    - [Sixten: Functional programming with fewer indirections](https://github.com/ollef/sixten)
        - It also deals with representing algebraic data type inhabitants as bit patterns.
    - https://en.wikipedia.org/wiki/Curry_(programming_language)
    - http://fsl.cs.illinois.edu/images/5/5e/Cayenne.pdf
- Extending Haskell
    - common problem in parsing: how to decorate AST in a functional language
        - 2017, "Trees that grow", https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf
            - "The compiler writer is then faced with two unpalatable choices.
            She can define a new data type representing the output decorated tree, at the cost of much duplication.
            Or she can write a single data type with all the necessary fields and constructors, at the cost of having many unused fields and constructors at different stages of compilation."
            - However, it is possible to achieve ADT extensibility with pattern synonyms, with Fix, and without type families.
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
    - Open ADTs (algebraic data types)
        - "Closed" means "defined in one place".
        - Open ADTs don't mix with exhaustive case analysis (function totality).
            - https://stackoverflow.com/questions/870919/why-are-haskell-algebraic-data-types-closed
            - But what if functions are "open" too?
                - https://www.andres-loeh.de/OpenDatatypes.pdf
        - If `f : a -> b`, then the compiler should infer `lift f : (Monad m) => m a -> m b`.
    - Can we extend Haskell to "auto-fmap"?
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
- [Extension programming language](extension.md)
- Every recursive type can be written as `mu a. F a` where F is the associated free functor?
    - Example: `List a = Fix (F a)` where `F a b = 1 + a * b`.
    - `Fix F = F (Fix F)` is the least fixed point of F.
- Haskell
    - language change proposals
        - Auto-lifting (and therefore sequencing) of function application involving Monad instances
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
    - Haskell supercompilation?
        - GHC
        - Supero
        - Morte
            - https://github.com/Gabriel439/Haskell-Morte-Library
                - "Morte is a super-optimizing intermediate language for functional languages."
            - http://www.haskellforall.com/2014/09/morte-intermediate-language-for-super.html
    - Dependency management
        - https://wiki.haskell.org/The_Monad.Reader/Issue2/EternalCompatibilityInTheory
            - It applies to all software, not only Haskell ones.
        - [Michael Snoyman's personal take on PVP version upper bounds](https://gist.github.com/snoyberg/f6f10cdbea4b9e22d1b83e490ec59a10)
    - curation
        - https://www.reddit.com/r/haskell/comments/4ggt05/best_underrated_haskell_libraries/
        - https://wiki.haskell.org/Applications_and_libraries
        - https://stackoverflow.com/questions/9286799/haskell-libraries-overview-and-their-quality
    - http://matt.might.net/articles/compiling-to-java/
    - unread
        - servant web framework
        - Salsa Haskell .NET bridge
            - https://wiki.haskell.org/Salsa
