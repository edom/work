-- | Integration.

module Sound.Int
(
    -- * Integration
    int
    , lint
    , rlint
    -- * Hints
    , Inp
    , Out
    , Sta
)
where

import Sound.InfList
import Sound.Hint
import Sound.Time

int :: (Num n) => StepSize n -> (Inp n, Sta n) -> (Out n, Sta n)
int d (x, y) = (y, y + x * d)
{-# INLINE int #-}

{- |
Numerically integrate the stream using Euler's method.

The first parameter is the step size.
-}
lint :: (Num a) => a -> L a -> L a
lint d =
    lscanl (\ y x -> y + x * d) 0
{-# INLINE lint #-}

-- | Numerical integral.
rlint :: (Fractional a) => RL a -> RL a
rlint x =
    rated r . lint d . unrated $ x
    where
        r = rate x
        d = recip . fromIntegral . _unRate $ r
{-# INLINE rlint #-}

type Inp a = a
type Out a = a
type Sta a = a
