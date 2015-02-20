module Sound.Step
(
    -- * Construction
    const
    , list
    , stream
    , proc
    , Step
    -- * Running
    , consume
    , Consumer
    -- * Specialized consumption
    , fill
    , Index
    , Count
)
where

import Control.Applicative
import Foreign
import Prelude hiding (const)

import Sound.InfList

{- |
An inhabitant of Step s a is a proof that s generates a.

The name is purposely meaningless.
Just follow the types to get an inhabitant, and then move on.
-}
newtype Step s a = MkStep { _unStep :: s -> IO (a, s) }
{-
Step is actually specialized StateT:
StateT s IO a = Step s a
-}

type Consumer a = Index Int -> a -> IO ()
type Index a = a
type Count a = a

consume :: Step s a -> Consumer a -> Count Int -> s -> IO s
consume gb eat n =  
    loop 0
    where
        loop i s
            | i >= n = return s
            | otherwise = do
                (x, s') <- unStep gb s
                eat i x
                loop (i + 1) s'
{-# INLINE consume #-}

-- | Run the generator to fill the buffer.
fill :: (Storable a) => Step s a -> Ptr a -> Count Int -> s -> IO s
fill gb ptr = consume gb (pokeElemOff ptr)
{-# INLINE fill #-}

mkStep :: (s -> IO (a, s)) -> Step s a
mkStep = MkStep

unStep :: Step s a -> (s -> IO (a, s))
unStep = _unStep

stream :: Step (L a) a
stream = mkStep f
    where
        f (MkL x y) = pure (x, y)

proc :: Step (IO a) a
proc = mkStep f
    where
        f s = fmap (flip (,) s) s

const :: a -> Step () a
const x = mkStep f
    where
        f y = pure (x, y)

list :: a -> Step [a] a
list def = mkStep f
    where
        f [] = pure (def, [])
        f (x:y) = pure (x, y)
