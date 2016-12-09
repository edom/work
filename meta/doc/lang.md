- Every functional programming language is lambda calculus plus plus.
- Software is executable mathematics.
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