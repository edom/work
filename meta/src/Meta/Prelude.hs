{- |
This reexports from "Prelude" the names that don't clash with our names.
-}
module Meta.Prelude (
    module Prelude
    , (P.|>)
) where

import Prelude hiding (
        div
        , exp
        , seq
        , span
    )

import qualified Meta.Prop as P
