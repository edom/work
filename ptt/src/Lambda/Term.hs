module Lambda.Term
(
    -- * Polymorphic term

    Term(..)
)
where

{- |
If you give us a boilerplate instance of this class,
we will beta-reduce lambda calculus terms for you.
-}
class Term t where

    -- | Construct a lambda application.
    mkApp :: t -> t -> t

    -- | Deconstruct a lambda application.
    unApp :: (t -> t -> a) -> (t -> a) -> (t -> a)

    -- | Construct a lambda abstraction.
    mkLam :: String -> t -> t

    -- | Deconstruct a lambda abstraction.
    unLam :: (String -> t -> a) -> (t -> a) -> (t -> a)

    -- | Construct a variable.
    mkVar :: String -> t

    -- | Deconstruct a variable.
    unVar :: (String -> a) -> (t -> a) -> (t -> a)
