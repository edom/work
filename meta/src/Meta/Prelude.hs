{- |
This mostly reexports symbols from @base@, for both backward and forward compatibility.

Usage:

@
import Prelude ()
import Meta.Prelude
@

or

@
import qualified Meta.Prelude as P
@
-}
module Meta.Prelude (
    module Meta.PreludeMin
    -- * Bool
    , not
    , and
    , or
    -- * List
    , concat
    , all
    , any
    , concatMap
    , filter
    , foldl
    , foldr
    , null
    , length
    , reverse
    -- ** Char, String
    , lines
    , unlines
    , unwords
    , words
    -- * Ord
    , Ord((<), (>), (<=), (>=))
    -- * Num
    -- $num
    , Num((+), (-), (*), fromInteger, negate)
    , fromIntegral
    , Integral
    , Fractional
    , Real
    , Double
    , Float
    , Int
    -- * Bounded, Enum
    , Bounded(minBound, maxBound)
    , Enum
    -- * IO
    , FilePath
    , IO
    , getLine
    , putStr
    , putStrLn
    -- * Power-of-two Int variants
    , I.Int8
    , I.Int16
    , I.Int32
    , I.Int64
    , W.Word8
    , W.Word16
    , W.Word32
    , W.Word64
    -- * Monad fail
    , fail
    -- * Custom
    , (|>)
) where

import Prelude
import Meta.PreludeMin

import qualified Data.Int as I
import qualified Data.Word as W

{- |
Flipped function application.
Flipped '$'.

https://github.com/izdi/elm-cheat-sheet
-}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>
