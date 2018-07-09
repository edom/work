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
    , (&&)
    , (||)
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
    , L.isPrefixOf
    , L.isInfixOf
    , L.isSuffixOf
    -- ** Char, String
    , lines
    , unlines
    , unwords
    , words
    , UpDownCase(..)
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
    , Os.Slurp(..)
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
    -- * Errors
    , user_error
    , raise_either
    -- * Function
    , (|>)
) where

import Prelude
import Meta.PreludeMin

import qualified Data.Char as C
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Word as W
import qualified System.IO.Error as IE

import qualified Meta.Os as Os

{- |
Flipped function application.
Flipped '$'.

https://github.com/izdi/elm-cheat-sheet
-}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

class UpDownCase a where
    upcase :: a -> a
    downcase :: a -> a

instance UpDownCase Char where
    upcase = C.toUpper
    downcase = C.toLower

instance (UpDownCase a) => UpDownCase [a] where
    upcase = map upcase
    downcase = map downcase

-- | @user_error msg = 'IE.ioError' ('IE.userError' msg)@.
user_error :: String -> IO a
user_error = IE.ioError . IE.userError

raise_either :: (Monad m) => Either String a -> m a
raise_either = either fail return
