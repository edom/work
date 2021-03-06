# Extension programming language?

## Motivation?

### The compiler should automate all universal properties?

Consider the example universal property in page 1 of the 1998 MacLane book "Categories for the working mathematician".

Let p : X * Y -> X and q : X * Y -> Y be the projections of the Cartesian product X * Y.
Those projections are called "universal", but why?

Suppose the programmer has defined a function f : W -> X * Y, where X /= Y.

The compiler should allow f to be used everywhere a W -> X is required or a W -> Y is required.
The compiler should automatically use p . f and q . f in place of f in such contexts.

Observe that an X * Y can be used everywhere an X is expected.
Thus we say that X * Y is a *subtype* of X, although X * Y is a *superspace* (extension) of X, because X * Y has more inhabitants than X.

Problem: X * X can't be used where an X is expected, because there are two possible projections, and the compiler can't know which one the programmer wants.

There are many injections X -> X * Y, but there is only one injection X * Y -> X.
If there is only one possible injection from A to B, then the compiler should allow the programmer to state that he wants the compiler to allow him to provide an A wherever a B is expected, and to use the injection automatically.

## Semantics of types? "Semantics" or "model"?

[Types form a *ring*](typering.md), like the natural numbers.
There we define an algebra of (mono)types.

There are some choices when defining S(A), the semantics of a type A.

- As the set S(A) = { S(x) | x : A }.
Here S(A) means the semantics of the type A, and S(x) means the (denotational semantics of the value x whose type is A.
- As the category S(A).
The objects are the set above.
The arrows are the endofunctions of A.

Properties:
- S(A * B) = S(A) * S(B), where the right * is Cartesian product.
- S(A + B) = S(A) + S(B), where the right + is set union.
- S(A -> B) = the computable subset of S(A) -> S(B)
- S (F A) = (S F) (S A), where F : Type -> Type.
- S(A (+) B) = S(A) (+) S(B) (disjoint union)
    - A (+) B = {0} * A + {1} * B
- S(0) = {} (the empty set)
- S(1) = {unit} (a singleton set, modulo isomorphism)
- S(Natural) = the set of all natural numbers
- S(Integer) = the set of all integers

How is a functional language a category?
See 1990 Barr & Wells book "Category theory for computing science" section 2.2 "Functional programming languages as categories" (page 20 onwards).
[Reprint](http://emis.ams.org/journals/TAC/reprints/articles/22/tr22a.pdf).
[Backup link](http://www.math.mcgill.ca/triples/Barr-Wells-ctcs.pdf): an older version, no big changes.

Let L be a functional programming language.
Let C be the category that is a model of such L.
An object in C is a type in L.
An arrow from A to B is an expression whose type is `A -> B`.

A functor F maps every object A to F(A),
and maps every arrow f : A -> B to F(f) : F(A) -> F(B).

Let F(A) = A + K describe a disjoint union.
Then F is an endofunctor.

```
F : Type -> Type
F A = A + K
f : A -> B

F : (A -> B) -> (F(A) -> F(B))
F f : A + K -> B + K
F f (inl a) = inl (f a)
F f (inr k) = inr k
```

We can model a functor in our language as:

```
-- This Functor actually corresponds an endofunctor in the category that is the model of this functional language, not just any arbitrary functor.
TYPE Functor = FIELDS
    F : Type -> Type
    m : (a -> b) -> (F a -> F b)
    -- Can the compiler prove this for us?
    prop : m (@id a) = @id (F a)
    prop = auto

TYPE NatTrans (S : Functor) (T : Functor) = FIELDS
    tau : S -> T

VAL Functor_identity : Functor = FIELDS
    F = id
    m = id

VAL Functor_add_right (K : Type) : Functor = FIELDS
    F a = a + K
    m f (inl a) = inl (f a)
    m f (inr k) = inr k

inc : Integer -> Integer
inc x = x + 1

input : Integer + String
input = 1    -- compiler infers auto-injection using inl

output : Integer + String -> Integer + String
output = inc input  -- compiler infers auto-injection, replaces this with `output = instance_Functor_Either.map inc input`
```

There is only one such functor.
The compiler should derive such functor for us.
For example: GHC can derive a Functor instance.

The functor F is also an object in C(L).

The functor category of C(L) is inside C(L) itself?

There is only one function that has the type `forall a b, a * b -> a`.
That function is `f (x,y) = x`.

```
INJECT EVERY (x, y) : a * b TO x : a
INJECT EVERY (x, y) : a * b TO y : b

FORALL a b x INJECT x : a TO Left x : Either a b
FORALL a b y INJECT y : b TO Right y : Either a b

TYPE F a = CHOICES
    | F0 a
    | F1 a

VAL a : Integer * Integer = (1, 2) -- OK
VAL _ : Integer = a -- Error: Ambiguous injections.
VAL _ : Either Integer Integer = 10 -- Error: Ambiguous injections.
VAL _ : F Integer = 1 -- Error: Ambiguous injections.
VAL _ : Maybe (Maybe Integer) = 10 -- OK
```

Example of natural transformation:

```
-- S = Maybe
-- T = Either String
nt : Maybe c -> Either String c
nt Nothing = Left ""
nt (Just x) = Right x
```

Example of an adjunction:

```
Adjunction from X = C(L) to A = C(L)
    F endofunctor of C(L)
    G endofunctor of C(L)
    phi_xa : (F x -> a) -> (x -> G a)
    phi_ax : (x -> G a) -> (F x -> a)
    such that phi_xa and phi_ax are inverses

Non-example:
    Let F = Maybe and G = Maybe
    phi_xa : (Maybe x -> a) -> (x -> Maybe a)
    phi_xa f = \ x -> Just (f (pure x))
    phi_ax : (x -> Maybe a) -> (Maybe x -> a)
    phi_ax f = \ mx -> case mx of
        Just x -> case phi_ax of
            Just a -> a
            Nothing -> ???
        Nothing -> ???
```

Example of a monad:

```
Let T be an endofunctor, which consists of
    T : Type -> Type
    and
    Tf :  (a -> b) -> (T a -> T b)
    .
eta : c -> T c
mu : T (T x) -> T x

and some laws

Example:
eta : a -> Maybe a
eta = Just

mu : Maybe (Maybe a) -> Maybe a
mu (Just x) = x
mu _ = Nothing
```

If M is a monad, then there is only one way to inject A to M A.

Hypothesis:
Cartesian-closed categories have such name because they are closed with respect to the Cartesian product:
For each of pair of objects in C, their Cartesian product is also an object in C.

https://en.wikipedia.org/wiki/De_Bruijn_index

2007, "A Head-to-Head Comparison of de Bruijn Indices and Names", https://www.sciencedirect.com/science/article/pii/S1571066107002319

## Drafts

An extension enables data types and functions to be extended automatically.

Notes about the word "extend".

LSP = Liskov substitution principle

- Difference with OOP (object-oriented programming):
    - In OOP, if A extends B (A is a subclass of B), then A is a subtype of B, provided that the programmer doesn't violate LSP.
    - Here, if A extends B (B is a subspace of A), then A is a subtype of B.
        - Interpret "extend" here as "grow", "widen", or "broaden".
        Think of adding objects into sets/spaces of mathematical objects.
        For example, you get the set of integers by *extending* the set of natural numbers with negative integers.
        Forget about OOP classes.
        - It would be nice if we can prove that all extensions in the language satisfy the LSP.
- What does "A is a subtype of B" mean?
    - Everywhere an A is required, a B can be substituted.
    - Idea: Subtyping is defined by universal property?
Mnemonic: B comes after A, so B is bigger than A.
- There are two subtypings:
    - Contra-subspacing (contravariant) subtyping:
        - A is a subtype of B iff S(A) is a superspace of S(B).
        - Example:
            - Suppose that Employee is a subtype of Person.
            - Then Employee -> Integer is a subtype of Person -> Integer.
            - S(Employee -> Integer) >= S(Person -> Integer).
    - Co-subspacing (covariant) subtyping:
        - A is a subtype of B iff S(A) is a subspace of S(B).
        - Example:
            - Natural is a subtype of `forall a. a`.
            - S(Natural) = Nat is a subspace of S(forall a. a) = Omega.

Consider:

```
TYPE Maybe a = CHOICES
    | Nothing
    | Just a

TYPE Nullable a = CHOICES
    | Null
    | Present a

INJECT EVERY x : a TO Present x : Nullable a
```

The semantics of `Maybe : Type -> Type` is:
    - S(Maybe) = s \mapsto {unit} + s.
    - S(Maybe a) = {unit} + S(a).

- Types and representation schemes
    - If T = U, then T and U should have the same representation scheme (same way of injecting mathematical values to bit patterns).
- How do we implement compile-type coproducts (untagged union) without runtime type information?
    - The compiler must be able to prove which branch of the coproduct is taken.
    For example, the compiler should be able to prove that `f 0 : Natural` and `n /= 0 |- f n : String`.
```
f : Natural -> Natural + String
f 0 = "Zero"
f n = n
```
    - Another example:
```
f0 : Natural -> Natural
f0 n = n + 1

f1 : String -> String
f1 x = "hello " ++ x

f : Natural + String -> Natural + String
f = coproduct f0 f1

f : Natural + String -> Natural
IF x : Natural
    THEN f x = x + 1
    ELSE f x = 0

-- No problem, because A + A = A, because types form a ring.
f : Natural + Natural -> Natural
f x = x + 1

-- f 0 should produce 1
-- f "john" should produce "hello john"
```
- A type declaration defines new syntax and the rules for type-checking that syntax.
```
SYNTAX
    IF a : Type THEN
        Nothing : Maybe a
        IF x : a THEN Just x : Maybe a
```
- Syntax:
    - A *word* is a string of non-whitespace characters.
    - A term is a sequence of words.
- Can an algebraic operation on types be translated automatically to operations on values?
    - For coproducts, yes.
    - How about product?
- These are the consequences of telling the compiler to extend A to B:
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
- Category-theoretic stuffs
    - Our programming language forms a category C where:
        - An object is a monotype.
        - A morphism is expression that denotes a function.
            - A morphism is an expression having type `a -> b`.
    - The act of adding a constructor forms an endofunctor.
        - Let T be an object of C.
        - Let K be an object of C.
        - We will show that M = (M_obj, M_mor) forms a functor.
        We will write M(A) to mean either M_obj(A) or M_mor(A) depending on context.
        This conflation is the norm in category theory.
        - Let M(T) = T+K be the type that results from adding the constructors of type K to type T.
        - Also define M_mor(f) (written M(f)) as follows:
            - M maps every Mor(A,B) (that is A -> B) into an Mor(M(A),M(B)) (that is A+K -> B+K) as follows:
                - M f (inl x) = f x
                - M f (inr y) = inr y
        - Show that M_mor is associative.
        - Thus M = (M_obj, M_mor) is an endofunctor.
    - The act of adding a parameter with default value forms an endofunctor.
    The act of adding a parameter forms a family of endofunctors.
        - Let F be an object of C.
        - Let M(T) = T*F be the type that results from adding a parameter of type F to every constructor of T.
        - Define a default value for F, call it def : F.
        - Define M(f) as M f x = (x, def).

The function `inject` is a special open function.
"Open" means that the definition isn't localized.
It can be spread.
The `INJECT` statement adds a case for the `inject` function.
The compiler inserts `inject` automatically,
but you can still explicitly insert `inject` manually.

Example: Numeric types:

```
TYPE Natural = CHOICES
    | Z
    | S Natural

TYPE Integer = CHOICES
    | P Natural
    | N Natural

-- These three statements are equivalent.
VAL inject : Natural -> Integer = P
INJECT USING P
INJECT EVERY Natural INTO Integer USING P
EMBED Natural IN Integer USING P

TYPE Rational = FIELDS
    , numerator : Integer -- dividend
    , denominator : Integer -- divisor

INJECT EVERY k : Integer INTO (k, 1) : Rational

EMBED Integer IN Rational
    BY INJECTING EVERY k : Integer TO (k, 1) : Rational

-- This representation of Real is questionable.
-- How can we print a Real?

TYPE FractionalPart digit = Natural -> digit

TYPE Real digit = (Integer, FractionalPart digit)
```

Alternative:

```
TYPE NaturalF a = CHOICES
    | Pure a
    | Succ a

TYPE Natural = Fix NaturalF

TYPE IntegerF a = CHOICES
    | Pure a
    | Negate a

TYPE Integer = Fix (IntegerF . NaturalF)

INJECT EVERY n : NaturalF a TO Pure n : IntegerF a
```

It would be nice if the language can help us define a fixpoint of a composition of functors with little boilerplate.

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

We don't automatically inject `b` into `Either a b` because doing so could introduce logic errors while refactoring.

```
TYPE Either a b = CHOICES
    | Left a
    | Right e
```

`Throws` is `Either` with automatic injection.

```
TYPE Throws e a = a WITH MORE CHOICES
    | Throw e

TYPE Throws e a = CHOICES
    | Throw e
    | Return a

ASSUMING e a
    EMBED a IN Throws e a
        BY INJECTING EVERY x TO Return x
```

```
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

VAL instance_Semigroup_List : Semigroup List = FIELDS
    Nil <> ys = ys
    Cons x xs <> ys = Cons x (xs <> ys)

DEFAULT Semigroup List = instance_Semigroup_List

-- what :: (Monad m) => m Int
VAL what : IMPLICIT instance : Monad m -> m Int =
    return instance

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
