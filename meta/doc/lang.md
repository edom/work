- Every functional programming language is lambda calculus plus plus.
- Church-encoding enables lambda calculus to represent conditionals and algebraic data types.
- Fixed-point combinators enables recursion and looping.
- https://en.wikipedia.org/wiki/Lambda_cube
- https://en.wikipedia.org/wiki/Calculus_of_constructions
- https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
    - "The simply typed lambda calculus [...], a form of type theory,
    is a typed interpretation of the lambda calculus with only one type constructor: [...] that builds function types."
        - What is an "interpretation of the lambda calculus"?
        - What is "the lambda calculus"? Is there only one lambda calculus?
- Parsing is the act of modifying the *state* of the parser.
    - 2017, ["A Typed, Algebraic Approach to Parsing"](https://www.cl.cam.ac.uk/~nk480/parsing.pdf)
        - "[...] we extend the notion of Brzozowski derivative from regular expressions to the typed context-free expressions."
- Lambda calculus semantics
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
- Implement lambda calculus.
    - Without dynamic allocation / garbage collection.
    - Translate lambda calculus to assembly
        - Basic idea:
            - Every expression translates to a subroutine.
            - Calling the subroutine ~ evaluating the expression.
            - Subroutine return value ~ value obtained by evaluating the expression.
        - A lambda abstraction translates to a subroutine that accepts one parameter.
        - An application translates to a subroutine call.
        - An int value translates to what? Choice:
            - itself
            - a subroutine that returns the int
- Other people's experiences
    - https://www.quora.com/If-you-were-to-design-a-programming-language-what-other-programming-language-would-it-evolve-from-and-what-aspect-of-that-language-would-you-do-differently
- parsing
    - given grammar, generate both parser and unparser/pretty-printer
        - http://www.semdesigns.com/Products/DMS/DMSPrettyPrinters.html?Home=DMSToolkit
        - https://hackage.haskell.org/package/invertible-syntax-0.2.1/src/Example.lhs
        - https://hackage.haskell.org/package/invertible-syntax
        - [Tillmann Rendel and Klaus Ostermann. "Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing". In Proc. of Haskell Symposium, 2010.](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)
        - http://jssst.or.jp/files/user/taikai/2016/PPL/ppl1-1.pdf
    - 2015, [Nez: practical open grammar language](https://arxiv.org/abs/1511.08307)
- Big names in programming language research, and their contributions
    - This list is incomplete.
    - See also:
        - https://en.wikipedia.org/wiki/Programming_language_theory
    - https://en.wikipedia.org/wiki/John_C._Reynolds
        - 2014, [The essence of Reynolds](http://www.cs.bham.ac.uk/~udr/papers/EssenceOfReynolds.pdf)
            - 1998, Reynolds, "Theories of programming languages"
    - Henk Barendregt, wrote books on lambda calculus
    - Thierry Coquand, invented calculus of constructions, Coq
    - Philip Wadler
    - Jean-Yves Girard
    - Steele, Sussman, Felleisen, Barzilay, etc. (Scheme guys, PLT Scheme guys)
    - Simon Peyton-Jones, etc. (Haskell guys)
    - Oleg Kiselyov
- books recommended by courses related to programming language research
    - http://plus.kaist.ac.kr/~han/courses/cs520/
    - https://www.cl.cam.ac.uk/teaching/1516/ConceptsPL/
- http://tomasp.net/academic/theses/coeffects/
- 2014, book, Odersky, "Scala by example"
    - perhaps the best way to learn Scala for non-Scala programmers
- ungrouped
    - https://medium.com/generative-design/introduction-to-computational-design-6c0fdfb3f1
    - magic?
        - http://conal.net/blog/posts/semantic-editor-combinators
    - [Salon des Refusés 2017](https://2017.programmingconference.org/track/refuses-2017)
    - [github.com/PyCQA/baron](https://github.com/PyCQA/baron): "IDE allow you to refactor code, Baron allows you to write refactoring code."
- Haskell supercompilation?
    - GHC
    - Supero
    - Morte
        - https://github.com/Gabriel439/Haskell-Morte-Library
            - "Morte is a super-optimizing intermediate language for functional languages."
        - http://www.haskellforall.com/2014/09/morte-intermediate-language-for-super.html
- Make a programming language.
    - For writing metaprograms.
        - Must have clear semantics.
        - Don't have to be fast.
    - Features
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
            - Programming languages with programmable syntax
                - [Katahdin](http://chrisseaton.com/katahdin/)
            - Programming languages with macros
                - Common Lisp
                - Scheme
                - Kotlin?
                - Clojure?
        - overloading?
        - compiler toolkit?
            - Stratego/XT
            - TXL
            - Eclipse Xtext, Eclipse Xtend
            - ANTLR
            - YACC, Bison
            - Lex, Flex
    - Non-features
        - layout
        - implicit Prelude import
        - lowercase-uppercase requirement

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

- Java
    - What I think Java gets wrong
        - JVM does too much.
            - JVM bytecode verification
                - It's the compiler's job, not the VM's.
                - Does it even add any security?
                    - Isn't JNI a bigger security hole?
