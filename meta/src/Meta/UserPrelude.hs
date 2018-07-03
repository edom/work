{- |
This reexports from "Prelude" the names that don't clash with our names.
-}
module Meta.UserPrelude (
    module Prelude
    , (P.|>)
) where

import Prelude hiding (
        div
        , seq
        , span
    )

import qualified Meta.Prop as P
