- A goal of programming language research is to make a better programming language?
    - Do more with less.
    - *The* ultimate best programming language?
- Every functional programming language is lambda calculus plus plus.
- Computer (machine) is embodied formal system.
    - Assume no hardware fault.
- Software is executable mathematics.
- A program is an executable formal system.
    - Can we formalize this using Grue's map theory?
        - Could this be as revolutionary as types-as-propositions (Curry-Howard correspondence) that enables proof assistants?
        - How is "false" represented?
        - How is "true" represented?
        - How is "conjunction" represented?
- Church-encoding enables lambda calculus to represent conditionals and algebraic data types.
- Fixed-point combinators enables recursion and looping.
- https://en.wikipedia.org/wiki/Lambda_cube
- https://en.wikipedia.org/wiki/Calculus_of_constructions
- https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus
    - "The simply typed lambda calculus [...], a form of type theory,
    is a typed interpretation of the lambda calculus with only one type constructor: [...] that builds function types."
        - What is an "interpretation of the lambda calculus"?
        - What is "the lambda calculus"? Is there only one lambda calculus?
- https://www.reddit.com/r/haskell/comments/8els6f/why_are_combinators_as_powerful_as_full/
- https://math.stackexchange.com/questions/5639/the-power-of-lambda-calculi
- interoperation between proof assistants?
    - Lem ("lightweight executable mathematics")
        - https://www.openhub.net/p/lightweight-executable-mathematics
        - http://www.cl.cam.ac.uk/~pes20/lem/
- foundation of mathematics
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
- Composition, composable systems
    - https://en.wikipedia.org/wiki/Software_transactional_memory#Composable_operations
    - composable grammars?
        - 1990, ["A Primer for Program Composition Notation"](https://authors.library.caltech.edu/26726/)
        - https://stackoverflow.com/questions/953185/composable-grammars
        - OMeta
        - Katahdin
        - "Parsing ought to be easier"
            - https://news.ycombinator.com/item?id=2330830
                - "PEGs are one class of cleanly composable grammars."?
        - cryptographic protocol analysis
            - https://en.wikipedia.org/wiki/Universal_composability
        - http://www.cs.cmu.edu/~Compose/
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
    - 2012, article, ["From Mathematics to Abstract Machine: A formal derivation of an executable Krivine machine"](https://arxiv.org/abs/1202.2924)
        - https://en.wikipedia.org/wiki/Krivine_machine
- Other people's experiences
    - https://www.quora.com/If-you-were-to-design-a-programming-language-what-other-programming-language-would-it-evolve-from-and-what-aspect-of-that-language-would-you-do-differently
- syntax, grammar, parsing, pretty-printing
    - Parsing is the act of modifying the *state* of the parser.
        - 2017, ["A Typed, Algebraic Approach to Parsing"](https://www.cl.cam.ac.uk/~nk480/parsing.pdf)
            - "[...] we extend the notion of Brzozowski derivative from regular expressions to the typed context-free expressions."
    - Unparsing is the linearization (serialization) of a tree.
    - Parsing is the treeization (delinearization, deserialization) of a line.
    - Parsing is String -> Maybe Tree.
    - Unparsing is Tree -> String.
    - Can we make parsing truly one-to-one? String -> Tree. CST = AST. Very rigid syntax. Forbid whitespace freedom.
    - Why lex (lexical analysis / tokenization)?
        - Cleaner grammar (no need to mention whitespaces in the grammar).
            - Example:
                - With lexing: `exp ::= exp "+" exp`
                - Without lexing: `exp ::= exp white "+" white exp`.
        - https://en.wikibooks.org/wiki/Compiler_Construction/Lexical_analysis
            - "Strictly speaking, tokenization may be handled by the parser. The reason why we tend to bother with tokenising in practice is that it makes the parser simpler,
            and decouples it from the character encoding used for the source code."
    - given grammar, generate both parser and unparser/pretty-printer
        - http://www.semdesigns.com/Products/DMS/DMSPrettyPrinters.html?Home=DMSToolkit
        - https://hackage.haskell.org/package/invertible-syntax-0.2.1/src/Example.lhs
        - https://hackage.haskell.org/package/invertible-syntax
        - [Tillmann Rendel and Klaus Ostermann. "Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing". In Proc. of Haskell Symposium, 2010.](http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf)
        - http://jssst.or.jp/files/user/taikai/2016/PPL/ppl1-1.pdf
    - Parsing Expression Grammar (PEG)
        - https://github.com/harc/ohm/
            - https://ohmlang.github.io/
                - https://harc.ycr.org/project/ohm/
        - Packrat
    - 2015, [Nez: practical open grammar language](https://arxiv.org/abs/1511.08307)
    - algebra of pretty-printing
        - 1995, Hughes, "The design of a pretty-printing library"
        - 1998, Wadler, "A prettier printer"
        - Hughes, Peyton-Jones, et al., http://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint-HughesPJ.html
    - [Efficient simple pretty printing combinators](https://www.cs.kent.ac.uk/people/staff/oc/pretty.html)
    - Earley parser
        - https://en.wikipedia.org/wiki/Earley_parser
        - https://hackage.haskell.org/package/Earley
    - https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#parsing--pretty-printing
        - https://hackage.haskell.org/package/trifecta
        - https://hackage.haskell.org/package/parsers
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
        - 1999, Steele, "Growing a language"
            - http://www.catonmat.net/blog/growing-a-language-by-guy-steele/
    - Simon Peyton-Jones, etc. (Haskell guys)
    - Oleg Kiselyov
    - Edward Kmett
    - Adam Chlipala
- paradigm, approach, viewpoint, worldview
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
- books recommended by courses related to programming language research
    - http://plus.kaist.ac.kr/~han/courses/cs520/
    - https://www.cl.cam.ac.uk/teaching/1516/ConceptsPL/
    - https://en.wikipedia.org/wiki/Programming_language_theory#Further_reading
- 2017, PhD thesis, ["Context-aware programming languages"](http://tomasp.net/academic/theses/coeffects/)
    - [amazingly empathic considerate presentation that has the audience in mind](http://tomasp.net/coeffects/)
        - if only all PhD theses are presented that way
            - if only everybody writes that way
- 2014, book, Odersky, "Scala by example"
    - perhaps the best way to learn Scala for non-Scala programmers
- ungrouped
    - https://medium.com/generative-design/introduction-to-computational-design-6c0fdfb3f1
    - magic?
        - http://conal.net/blog/posts/semantic-editor-combinators
    - [Salon des Refusés 2017](https://2017.programmingconference.org/track/refuses-2017)
    - refactoring
        - [github.com/PyCQA/baron](https://github.com/PyCQA/baron): "IDE allow you to refactor code, Baron allows you to write refactoring code."
        - HaRe Haskell refactoring tool https://github.com/alanz/HaRe
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
                - Scala? https://www.scala-lang.org/blog/2017/11/27/macros.html
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
    - What others think Java gets wrong
        - http://tech.jonathangardner.net/wiki/Why_Java_Sucks
    - What I'm not sure about Java
        - Why type erasure?
            - C# 2.0 introduced generics, without type erasure, while maintaining backward compatibility.
                - Duplicating everything in System.Collections to System.Collections.Generic is ugly, but it's less ugly than type erasure.
                - https://docs.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-version-history
                - https://stackoverflow.com/questions/11436802/how-are-c-sharp-generics-implemented
                    - https://blogs.msdn.microsoft.com/ericlippert/2009/07/30/whats-the-difference-part-one-generics-are-not-templates/
- What I think every procedural programming language (Java, C, C++, Go, Python, Ruby) gets wrong, except JavaScript
    - Functions should be first-class. Methods should not exist. Fields are enough.
    - It should be possible to assign to methods.
    - `a.b()` should not mean "call method b of object a"
    - `a.b()` should mean "get the function a.b, then call it".
    - `a.b()` should mean `(a.b)()`.
        - `m = a.b; m();` should mean the same as `a.b()`.
    - JavaScript does this right.
    - What JavaScript does wrong, TypeScript does right.
- What I think C and C++ got right but get wrong
    - In the 1970s these were tolerable (memory was limited; tools didn't exist). In 2018 these aren't tolerable.
        - Writing header files manually (should be generated from C files instead)
        - Preprocessor that works on text, not on C/C++ AST
- obscure
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
- What I think Haskell gets wrong
    - Template Haskell
        - Instead, we should have a metaprogramming library, and generate hs files.
    - Build is too slow.
        - Where should we fix this? GHC? Cabal?
- What I think every imperative programming language gets wrong
    - Statements vs expressions
        - Statement should not exist. Only expressions should.
            - Eclipse Xtend does this right.
                - https://www.eclipse.org/xtend/documentation/203_xtend_expressions.html
            - We can use lambda calculus for imperative programming.
                - Treat the statement separator (here we use semicolon `;`)
                as an operator (function with infix syntax)
                that sequences the effects of two expressions.
                    - For example:
                        - Let `a` be an expression.
                        - Let `b` be an expression.
                        - Thus `a;b` is an expression.
                        - The value of `a;b` is the value of `b`.
                        - The effect of `a;b` is the effect of `a` followed by the effect of `b`.
                    - Semicolon is associative: `(a;b);c = a;(b;c)`. This also holds for the effects.
            - Can we replace statements with expressions in existing imperative languages without breaking backward compatibility?
        - But C has https://en.wikipedia.org/wiki/Comma_operator
- IDE (integrated development environment), editor
    - https://hackage.haskell.org/package/treersec "Build a structure editor for a programming language from combinators following the grammar."
    - http://www.lamdu.org/
    - cirru https://news.ycombinator.com/item?id=13773813
        - lots of similar things https://news.ycombinator.com/item?id=13774864
    - isomorf: find code fragment popularity by structure (not-only-text) comparison https://isomorf.io/#!/tours/of/overview/7/haskell
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
