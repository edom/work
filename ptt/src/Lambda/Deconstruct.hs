module Lambda.Deconstruct
(
    -- * Convenient polymorphic deconstruction

    app
    , lam
    , var

    -- * Combinator

    , (|>)
    , (<!>)

    -- ** From "Control.Applicative"

    , Alternative
    , (<|>)
)
where

import Control.Applicative
    (
        Alternative
        , empty
        , (<|>)
    )

import Data.Maybe
    (
        fromMaybe
    )

import Lambda.Term

-- | Deconstruct a lambda application.
app :: (Alternative m, Term t) => (t -> m a) -> (t -> m b) -> (t -> m (a, b))
app f g = unApp (\ x y -> (,) <$> f x <*> g y) (const empty)

-- | Deconstruct a lambda abstraction.
lam :: (Alternative m, Term t) => (String -> m a) -> (t -> m b) -> (t -> m (a, b))
lam f g = unLam (\ x y -> (,) <$> f x <*> g y) (const empty)

-- | Deconstruct a variable.
var :: (Alternative m, Term t) => t -> m String
var = unVar pure (const empty)

-- | Flipped '<$>' with the same precedence level but flipped fixity.
(|>) :: (Functor f) => f a -> (a -> b) -> f b
(|>) = flip (<$>)
infixr 4 |>

-- | Flipped 'fromMaybe' that has the same fixity as that of '<|>'.
(<!>) :: Maybe a -> a -> a
(<!>) = flip fromMaybe
infixl 3 <!>
