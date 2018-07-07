{- |
Not to be used directly.
-}
module Meta.PreludeMin (
    -- * Bool
    Bool(False, True)
    -- * Char, String
    , Char
    , String
    -- * Functor, Applicative, Monad
    -- $amp
    , Functor(fmap)
    , (A.<$)
    , (A.<$>)
    , A.Applicative((<*>), pure)
    , (A.*>)
    , (A.<*)
    , A.Alternative((<|>))
    , Monad((>>), (>>=), return)
    , (=<<)
    , mapM
    , mapM_
    , M.forM
    , M.forM_
    , M.unless
    , M.replicateM
    , M.replicateM_
    -- ** Either, Maybe
    , Either(Left, Right)
    , either
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
    , Read
    , Show(show)
    , readEither
    , readMaybe
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
