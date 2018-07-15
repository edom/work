{-# LANGUAGE FunctionalDependencies #-}
{- |
Not to be used directly.
-}
module Meta.PreludeMin (
    -- * Bool
    Bool(False, True)
    , otherwise
    -- * Char, String
    , Char
    , String
    -- * Functor, Applicative, Monad
    -- $amp
    -- ** Functor
    , Functor(fmap)
    -- *** Functions
    , (A.<$)
    , (A.<$>)
    -- ** Applicative
    , A.Applicative((<*>), pure)
    -- *** Functions
    , (A.*>)
    , (A.<*)
    -- ** Alternative
    , A.Alternative((<|>))
    -- ** Monad
    , Monad((>>), (>>=), return)
    -- *** Functions
    , (=<<)
    , mapM
    , mapM_
    , M.forM
    , M.forM_
    , M.when
    , M.unless
    , M.replicateM
    , M.replicateM_
    , (M.>=>)
    -- * Either, EitherString, Maybe
    -- ** Either
    , Either(Left, Right)
    -- ** EitherString
    , EitherString
    -- ** Functions
    , C_either(..)
    -- ** Maybe
    , Maybe(Nothing, Just)
    , maybe
    -- * Function, error, tuple
    , ($)
    , (.)
    , const
    , id
    , flip
    , curry
    , uncurry
    , error
    , fst
    , snd
    -- * Read, Show, polyfills
    -- $read
    -- ** Read
    , Read
    -- *** Functions
    , readEither
    , readMaybe
    -- ** Show
    , Show(show)
    -- * Eq
    , Eq((==), (/=))
    -- * Ord
    , Ord
    -- * List
    , (++)
    , map
    , take
    , drop
) where

import Prelude hiding (either)
import qualified Prelude as P

import qualified Control.Applicative as A
import qualified Control.Monad as M

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

{- $read
The 'Text.Read.readEither' and 'Text.Read.readMaybe' in "Text.Read" was introduced in base 4.6 (GHC 7.6).

Our polyfills here may differ from those.

https://en.wikipedia.org/wiki/Polyfill_(programming)
-}

readEither :: (Read a) => String -> Either String a
readEither str = case reads str of
    [(a,"")] -> a
    [] -> Left "Prelude.read: No parse"
    _ -> Left "Prelude.read: Ambiguous parse"

readMaybe :: (Read a) => String -> Maybe a
readMaybe = either (const Nothing) Just . readEither

{- |
@'EitherString' a@ is @'Either' 'String' a@ whose 'fail' does the expected thing.

EitherString is also an instance of 'A.Alternative' and 'M.MonadPlus'.

Use 'either' from 'C_either' to pattern-match an EitherString.
-}
newtype EitherString a = MkEitherString { unEitherString :: Either String a }
    deriving (Eq, Ord, Read, Show)

instance Functor EitherString where
    fmap f = MkEitherString . fmap f . unEitherString

instance A.Applicative EitherString where
    pure = MkEitherString . A.pure
    (<*>) a b = MkEitherString (unEitherString a A.<*> unEitherString b)

instance A.Alternative EitherString where
    empty = MkEitherString (Left "")
    (<|>) a b = MkEitherString (go (unEitherString a) (unEitherString b))
        where
            go x@(Right _) _ = x
            go _ y = y

instance Monad EitherString where
    fail = MkEitherString . Left
    return = A.pure
    (>>=) a b = MkEitherString (unEitherString a >>= unEitherString . b)

instance M.MonadPlus EitherString where
    mzero = A.empty
    mplus = (A.<|>)

{- |
This generalizes 'P.either' from "Prelude" so that it works with both 'Either' and 'EitherString'.

All instances of this class must be in the module "Meta.PreludeMin".
Don't define your own instances.
-}
class C_either e a b | e -> a b where
    either :: (a -> c) -> (b -> c) -> e -> c

instance C_either (Either a b) a b where
    either = P.either

instance C_either (EitherString b) String b where
    either a b = P.either a b . unEitherString
