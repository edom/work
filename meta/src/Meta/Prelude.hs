{- |
This reexports from "Prelude" the names that don't clash with our names.

Usage:

@
import Prelude ()
import Meta.Prelude
@
-}
module Meta.Prelude (
    module Prelude
    , module Control.Applicative
    -- * "Text.Read"
    -- $read
    , readEither
    , readMaybe
    -- * Custom
    , (|>)
    -- * Notes for GHC 7.8 and the AMP
    -- $amp
) where

import Prelude hiding (
        div
        , exp
        , seq
        , span
    )

import Control.Applicative (
        (*>)
        , (<$>)
        , (<*)
        , (<*>)
        , Applicative
        , pure
    )

{- |
Flipped function application.
Flipped '$'.

https://github.com/izdi/elm-cheat-sheet
-}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

{- $read
The 'Text.Read.readEither' and 'Text.Read.readMaybe' in "Text.Read" was introduced in base 4.6 (GHC 7.6).

Our version here may differ from those.
-}

readEither :: (Read a) => String -> Either String a
readEither str = case reads str of
    [(a,"")] -> a
    [] -> Left "Prelude.read: No parse"
    _ -> Left "Prelude.read: Ambiguous parse"

readMaybe :: (Read a) => String -> Maybe a
readMaybe = either (const Nothing) Just . readEither

{- $amp
This module does not fix the AMP (Applicative-Monad Proposal) that took effect since GHC 7.8.

For example, this fragment doesn't compile with GHC older than 7.8:

@
foo :: ('Monad' f) => f (a -> b) -> f a -> f b
foo = ('<$>')
@

while this fragment is more backward-compatible:

@
foo :: ('Functor' f, 'Monad' f) => f (a -> b) -> f a -> f b
foo = ('<$>')
@

This is also backward-compatible:

@
foo :: ('Monad' f) => f (a -> b) -> f a -> f b
foo f x = x '>>=' 'return' '.' f
@

-}
