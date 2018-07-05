{- |
This reexports from "Prelude" the names that don't clash with our names.

This also serves as a compatibility layer for GHC 7.8 and older
before the Applicative-Monad Proposal took effect.

@
import Prelude ()
import Meta.Prelude
@
-}
module Meta.Prelude (
    module Prelude
    , module Control.Applicative
    , (|>)
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
