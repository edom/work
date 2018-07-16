Extension programming language

An extension enables data types and functions to be extended automatically.

The semantics of a type is a set.
Let \( A \) be a type.
Let \( B \) be a type.
Iff \( B \) extends \( A \), then \( S(A) \subseteq S(B) \).
Mnemonic: B comes after A, so B is bigger than A.

Notes about the word "extend".

LSP = Liskov substitution principle

Difference with OOP (object-oriented programming):
    - In OOP, if A extends B (A is a subclass of B), then A is a subtype of B, provided that the programmer doesn't violate LSP.
    - Here, if A extends B (B is a subspace of A), then B is a subtype of A.
        - Note the flipping.
        - Interpret "extend" here as "grow", "widen", or "broaden".
        Think of adding objects into sets/spaces of mathematical objects.
        For example, you get the set of integers by *extending* the set of natural numbers with negative integers.
        Forget about OOP classes.
        - It would be nice if we can prove that all extensions in the language satisfy the LSP.

These are the consequences of telling the compiler to extend A to B:
    - Everywhere an A is expected, a B can be substituted.
    - The compiler automatically does these:
        - Define an *injection* `i : A -> B` (an injective function).
        We may also call it an *embedding* from A to B because it injects A into a subspace of B.
            - An injection is a lossless mapping.
        - For every type t:
            - Extend every function f : t -> S to f' : t -> B.
            - Extend every function f : B -> t to f' : S -> t.
            - Don't forget that constructors are functions too.
    - There are two ways to auto-injectably extend a type T (there are two operations on types):
        - by adding constructors (additive/summative extension) (U is T plus some more constructors).
        - by adding parameters (multiplicative/productive extension) to every constructor of T.
    - Let T + C denote the type that results from adding constructor C to type T.
    - Let T * F denote the type that results from adding field F to type T.
    - For every function f : T -> U, the compiler should automatically infer these:
        - ? : T+C -> U
        - ? : T*F -> U
        - ? : T -> U+C
        - ? : T -> U*F, provided F has default value
        - For every M : * -> *, the compiler should infer (A -> B) -> (M A -> M B) ?
            - M T = T+C
                - ? : T+C -> T+C
            - M T = T*F
                - ? : T*F -> T*F, preserving the F
        - What is the category?
            - What is an object? A type?
            - What is a morphism? An expression of type a -> b?

Example: Numeric types:

```
TYPE Natural = CHOICES
    | Z
    | S Natural

TYPE Integer = CHOICES
    | P Natural
    | N Natural

INJECT EVERY Natural INTO Integer USING P

TYPE Rational = FIELDS
    , numerator : Integer -- dividend
    , denominator : Integer -- divisor

INJECT EVERY k : Integer INTO (k, 1) : Rational
-- INJECT EVERY Integer INTO Rational USING \ k -> Rational { numerator = k, denominator = 1, }

-- This representation of Real is questionable.
-- How can we print a Real?

TYPE FractionalPart digit = Natural -> digit

TYPE Real digit = (Integer, FractionalPart digit)
```

Example: `Maybe a`:

```
TYPE Maybe a = CHOICES
    | Just a
    | Nothing

INJECT USING Just
-- INJECT EVERY x : a INTO Just x : Maybe a

DEFAULT USING Nothing
-- DEFAULT EVERY Maybe a USING Nothing
-- DEFAULT INHABITANT OF Maybe a IS Nothing
-- TERMINAL OBJECT OF Maybe a IS Nothing -- or is it initial object?
```

Example of injection in action:

```
-- This compiles fine.
-- The compiler automatically injects the Int into Maybe Int using Just.
VAL x : Maybe Int = 1

-- The compiler automatically injects the Int into Maybe Int using Just.
-- Then compiler automatically injects the Maybe Int into Maybe (Maybe Int) using Just.
VAL y : Maybe (Maybe Int) = 1

VAL f : Int -> Int = \ x -> x + 1

-- This compiles fine.
-- For example, `g 1` evaluates to `Just 2`, and `g Nothing` evaluates to `Nothing`.
-- The compiler automatically extends `f : Int -> Int` to `f' : Maybe Int -> Maybe Int`.
VAL g = f x

```

Example of defaulting in action: `f Nothing` evaluates to `Nothing`.

`Nullable a` is similar to `Maybe a`.
The difference: for every `a`, `Nullable a` extends (is a subtype of) `a`.

```
TYPE Nullable a = a WITH MORE CHOICES
    | Null

DEFAULT USING Null      -- DEFAULT EVERY Nullable a USING Null

-- This compiles fine because every Int is a Nullable Int.
VAL val : Nullable Int = 1

-- This compiles fine.
-- Example evaluations with the above f:
-- f y = 3
-- f Null = Null
VAL y : Nullable (Nullable Int) = 2
```

- What is the type of Null?
    - Nullable a
    - Nullable (Nullable a)
    - Nullable (Nullable (Nullable a))
    - ...
    - Are those the same?

Conceptually the following `eval` is an inverse of extension: folding, flattening, crushing:

```
TYPE ExpArith e = CHOICE
    | Plus e e

TYPE ExpBool e = CHOICE
    | And e e

TYPE Exp e = Fix (\ e ->
        CHOICES
        | A (ExpArith e)
        | B (ExpBool e)
    )

INJECT USING A -- INJECT EVERY ExpArith INTO Exp USING A
INJECT USING B -- INJECT EVERY ExpBool INTO Exp USING B

TYPE Fix f = MkFix { unFix :: f (Fix f) }

INJECT f (Fix f) INTO Fix f USING MkFix
INJECT Fix f INTO f (Fix f) USING unFix
```

```
TYPE Either a b = CHOICES
    | Left a
    | Right e

TYPE Throws e a = a WITH MORE CHOICES
    | Throw e

TYPE List a = CHOICES
    | Nil
    | Cons a (List a)

TYPE Person = FIELDS
    , name : String
    , age : Int

TYPE Employee = Person WITH MORE FIELDS
    , company : String
```

Example of multiplicative extension:

```
-- A row in a database table.
TYPE Entry a = a WITH MORE FIELDS
    , creationTime : Time
    , modificationTime : Time

DEC save : Entry Employee -> IO ()
```

```
TYPE State s a = ALIAS s -> (s, a)

TYPE Writer w a = a WITH MORE FIELDS
    , output : w
```

Example: decorating AST, alternative to "Trees that grow":

```
TYPE Located a = a WITH MORE FIELDS
    , location : Location

-- Abstract syntax tree after type checking.
TYPE TcAst = Ast WITH MORE FIELDS
    , tcData : TcData
```

Type classes and instances?

```
TYPE Semigroup m = FIELDS
    , (<>) : m -> m -> m

TYPE Monoid m = Semigroup m WITH MORE FIELDS
    , empty : m

TYPE Functor m = FIELDS
    , map : (a -> b) -> m a -> m b

TYPE Monad m = Functor m WITH MORE FIELDS
    , return : m -> a
    , join : m (m a) -> m a

VAL instance_Functor_Maybe : Functor Maybe = FIELDS
    ,   map f (Just x) = Just (f x)
        map _ Nothing = Nothing

VAL instance_Monad_Maybe : Monad Maybe = instance_Functor_Maybe WITH MORE FIELDS
    ,   return = Just
    ,   join Nothing = Nothing
        join (Just x) = x

ASSUMING instance : Monad m
    INJECT EVERY a INTO m a USING instance.return
```
