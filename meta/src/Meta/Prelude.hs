{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    , Filter(..)
    , P.foldl
    , P.foldr
    , P.null
    , P.length
    , P.reverse
    , L.isPrefixOf
    , IsInfixOf(..)
    , L.isSuffixOf
    , beginsWith
    , endsWith
    -- ** Repetition
    , P.repeat
    , P.replicate
    , P.cycle
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
    -- * IO, transput
    , P.FilePath
    , P.IO
    , getLine
    , putStr
    , putStrLn
    -- ** Sloppy transput
    , Os.slurp
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
    -- * Operating system services
    -- ** Command-line arguments
    , Os.getArgs
    -- * Array-like, integer-indexable collection
    , At(..)
    , FromList(..)
    , ToList(..)
    -- * Data.Vector
    , V.Vector
    -- * Maps from Data.Map
    , Map.Map
    , Map.from_list
    , Map.fromListWith
    -- * Semigroup and Monoid
    , Sgr.Semigroup(..)
    , Mon.Monoid(..)
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
import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Sgr
import qualified Data.String as S
import qualified Data.Word as W
import qualified Prelude as P
import qualified System.IO.Error as IE

import qualified Control.Monad.IO.Class as IC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import qualified Meta.ByteString as B
import qualified Meta.Map as Map
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

class Filter c a | c -> a where
    filter :: (a -> Bool) -> c -> c

instance Filter [a] a where
    filter = P.filter

instance Filter (V.Vector a) a where
    filter = V.filter

class IsInfixOf a where
    isInfixOf :: a -> a -> Bool

instance (Eq a) => IsInfixOf [a] where
    isInfixOf = L.isInfixOf

instance IsInfixOf B.ByteString where
    isInfixOf = BS.isInfixOf

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

-- | Integer-indexed collection. See 'at'.
class At c i e where
    {- |
@at c i@ is the element at index @i@ of collection @c@.

This function is to be used infix such as @c `at` 123@.

For speed, use `at` with Vector from Data.Vector, and don't use `at` with lists.
    -}
    at :: c -> i -> e

instance (Monad m, P.Integral i, Show i, m a ~ b) => At [a] i b where
    at _ i | i P.< 0 = P.fail $ "at: negative index: " ++ show i
    at list index = go list index
        where
            go (h:_) 0 = return h
            go (_:t) n = go t (n P.- 1)
            go _ _ = P.fail $ "at: index out of bounds: " ++ show index

instance (Monad m, P.Integral i, Show i, m a ~ b) => At (V.Vector a) i b where
    at c i = maybe (P.fail $ "at: Vector: index out of bounds: " ++ show i) return $ c V.!? P.fromIntegral i

instance (Monad m, Ord k, Show k) => At (Map.Map k v) k (m v) where
    at c i = maybe (P.fail $ "at: Map: key not found: " ++ show i) return $ Map.lookup i c

class FromList a b where
    fromList :: a -> b

instance FromList [a] (V.Vector a) where
    fromList = V.fromList

class ToList a b where
    toList :: a -> b

instance (a0 ~ a1) => ToList (V.Vector a0) [a1] where
    toList = V.toList

instance (k0 ~ k1, a0 ~ a1) => ToList (Map.Map k0 a0) [(k1, a1)] where
    toList = Map.toList
