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
