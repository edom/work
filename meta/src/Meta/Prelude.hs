{-# LANGUAGE FunctionalDependencies #-}

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

* Principles:

    * Assume that the user just wants to get started quickly.

    * Therefore:

        * Be maximalist.
        Export as much as possible.

        * Be sloppy.
        Prioritize ease of use over everything else.

            * Avoid overzealous types.
            Don't pretend that Haskell has dependent types.

            * Avoid partial functions.
            They produce unhelpful error messages when they fail.

            * Inefficiency is acceptable.
            There are other packages if the user needs performance.

            * When working in IO, just throw an exception if anything fails.
            The user doesn't want to handle error.

* Similar packages:

    * https://hackage.haskell.org/package/classy-prelude

    * https://hackage.haskell.org/package/basic-prelude

    * https://hackage.haskell.org/packages/#cat:Prelude
-}
module Meta.Prelude (
    module Meta.PreludeMin
    -- * Bool
    , not
    , and
    , or
    , (&&)
    , (||)
    -- * Bitwise operations
    , (Bits..&.)
    , (Bits..|.)
    , Bits.xor
    , Bits.unsafeShiftL
    , Bits.shiftL
    , Bits.shiftR
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
    , beginsWith
    , endsWith
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
    , RealFloat(isNaN)
    , RealFrac(truncate)
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
    -- ** IORef
    , IORef.IORef
    , IORef.newIORef
    , IORef.readIORef
    , IORef.writeIORef
    , IC.MonadIO(liftIO)
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
    -- * Concurrency
    -- $concur
    -- ** Forking
    , Con.forkIO
    , Con.forkOS
    , Con.killThread
    -- * ByteString
    , B.ByteString
    , B.LazyByteString
    -- * List-like interface for things
    , UnconsPure(..)
    , UnconsA(..)
) where

import Prelude hiding (either)
import Meta.PreludeMin

import qualified Control.Applicative as A
import qualified Control.Concurrent as Con
import qualified Data.Bits as Bits
import qualified Data.Char as C
import qualified Data.IORef as IORef
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Word as W
import qualified System.IO.Error as IE

import qualified Control.Monad.IO.Class as IC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Meta.ByteString as B
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

-- | Synonym of 'L.isPrefixOf'.
beginsWith :: (Eq a) => [a] -> [a] -> Bool
beginsWith = L.isPrefixOf

-- | Synonym of 'L.isSuffixOf'.
endsWith :: (Eq a) => [a] -> [a] -> Bool
endsWith = L.isSuffixOf

{- $concur
This assumes that the compiler is GHC.

See also "Control.Concurrent".
-}

{- |
A generalized list is either a generalized nil or a generalized cons.
-}
class UnconsPure lst elm | lst -> elm where
    -- | Case analysis. Pattern matching.
    unconsPure
        :: lst -- ^ generalized list
        -> a -- ^ what to do if the generalized list is a generalized nil
        -> (elm -> lst -> a) -- ^ what to do if the generalized list is a generalized cons
        -> a

instance UnconsPure [elm] elm where
    unconsPure lst emp con = case lst of
        [] -> emp
        x:y -> con x y

instance UnconsPure B.ByteString W.Word8 where
    unconsPure lst emp con = maybe emp (uncurry con) $ BS.uncons lst

instance UnconsPure B.LazyByteString W.Word8 where
    unconsPure lst emp con = maybe emp (uncurry con) $ BSL.uncons lst

class UnconsA lst elm | lst -> elm where
    unconsA :: (A.Alternative f) => lst -> f (elm, lst)

instance UnconsA [elm] elm where unconsA = defUnconsA
instance UnconsA B.ByteString W.Word8 where unconsA = defUnconsA
instance UnconsA B.LazyByteString W.Word8 where unconsA = defUnconsA

defUnconsA
    :: (A.Alternative f, UnconsPure lst elm)
    => (lst -> f (elm, lst))

defUnconsA lst = unconsPure lst A.empty (curry A.pure)
