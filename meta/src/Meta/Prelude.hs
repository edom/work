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

    * <https://hackage.haskell.org/package/classy-prelude>

    * <https://hackage.haskell.org/package/basic-prelude>

    * <https://hackage.haskell.org/packages/#cat:Prelude>
-}
module Meta.Prelude (
    module Meta.PreludeMin
    -- * Bool
    , P.not
    , P.and
    , P.or
    , (P.&&)
    , (P.||)
    -- * Bitwise operations
    , (Bits..&.)
    , (Bits..|.)
    , Bits.xor
    , Bits.unsafeShiftL
    , Bits.shiftL
    , Bits.shiftR
    -- * List
    , P.concat
    , P.all
    , P.any
    , P.concatMap
    , P.filter
    , P.foldl
    , P.foldr
    , P.null
    , P.length
    , P.reverse
    , L.isPrefixOf
    , L.isInfixOf
    , L.isSuffixOf
    , beginsWith
    , endsWith
    -- ** Char, String
    , P.lines
    , P.unlines
    , P.unwords
    , P.words
    , UpDownCase(..)
    -- * Ord
    , P.Ord((<), (>), (<=), (>=))
    -- * Num
    -- $num
    , P.Num((+), (-), (*), fromInteger, negate)
    , P.fromIntegral
    , P.Integral
    , P.Fractional
    , P.Real
    , P.RealFloat(isNaN)
    , P.RealFrac(truncate)
    , P.Double
    , P.Float
    , P.Int
    -- * Bounded, Enum
    , P.Bounded(minBound, maxBound)
    , P.Enum
    -- * IO
    , P.FilePath
    , P.IO
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
    , P.fail
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
    -- * IsString class for OverloadedStrings extension
    , S.IsString(..)
    -- * Faithful functors
    , Faithful(..)
) where

import Prelude ()
import Meta.PreludeMin

import qualified Control.Applicative as A
import qualified Control.Concurrent as Con
import qualified Data.Bits as Bits
import qualified Data.Char as C
import qualified Data.IORef as IORef
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.String as S
import qualified Data.Word as W
import qualified Prelude as P
import qualified System.IO.Error as IE

import qualified Control.Monad.IO.Class as IC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Meta.ByteString as B
import qualified Meta.Os as Os

{- |
Flipped function application.
Flipped '$'.
But '$' binds more strongly than @|>@.

https://github.com/izdi/elm-cheat-sheet
-}
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 1 |>

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
user_error :: String -> P.IO a
user_error = IE.ioError . IE.userError

raise_either :: (Monad m) => Either String a -> m a
raise_either = either P.fail return

-- | @a `beginsWith` b = b `'L.isPrefixOf'` a@.
beginsWith :: (Eq a) => [a] -> [a] -> Bool
beginsWith = flip L.isPrefixOf

-- | @a `endsWith` b = b `'L.isSuffixOf'` a@.
endsWith :: (Eq a) => [a] -> [a] -> Bool
endsWith = flip L.isSuffixOf

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

-- | This generalizes 'P.getLine' from "Prelude".
getLine :: (IC.MonadIO m) => m String
getLine = IC.liftIO P.getLine

-- | This generalizes 'P.putStr' from "Prelude".
putStr :: (IC.MonadIO m) => String -> m ()
putStr = IC.liftIO . P.putStr

-- | This generalizes 'P.putStrLn' from "Prelude".
putStrLn :: (IC.MonadIO m) => String -> m ()
putStrLn = IC.liftIO . P.putStrLn

{- |
An instance of Faithful f means that it is in principle always possible to recover @x@ from @'faithful' x@.

It means that @f :: * -> *@ is an injective function, if we think of types as sets.

If @f@ is also an instance of 'A.Applicative', then it should also satisfy @pure = faithful@.

An example of a faithful data type:

@
data F a
    = Pure a
    | SomethingElse

faithful = Pure
@

An example of a forgetful (non-faithful) data type:

@
data G a
    = AnInt Int
    | SomethingFancy
@
-}
class Faithful f where
    faithful :: a -> f a
