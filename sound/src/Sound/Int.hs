{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Integration.
module Sound.Int
(
    -- * Integration
    Integrable(..)
    -- * Arrow-style
    , aint
    -- * Scan-style
    , sintc
    , sint
    , rlint
    -- * Hints
    , Inp
    , Out
    , Sta
)
where

import Control.Arrow

import Sound.Class
import Sound.InfList
import Sound.Hint
import Sound.Time

class Integrable f where
    int :: (Num n) => StepSize n -> f n -> f n

aint :: (Arrow a, Num n, Pair p) => StepSize n -> a (p (Inp n) (Sta n)) (p (Out n) (Sta n))
aint !d = arr $ uncurry $ \ !x !y -> mkPair y (y + x * d)
{-# INLINE aint #-}

-- | Resumable integration.
sintc :: (Num a, Scan f) => StepSize a -> a -> f a -> f a
sintc !dx = scanl (\ !y !fx -> y + fx * dx)
{-# INLINE sintc #-}

sint :: (Num a, Scan f) => StepSize a -> f a -> f a
sint !dx = sintc dx 0
{-# INLINE sint #-}

-- | Numerical integral.
rlint :: (Fractional a) => RL a -> RL a
rlint x =
    rated r . sint d . unrated $ x
    where
        r = rateOf x
        d = recip . fromIntegral . _unRate $ r
{-# INLINE rlint #-}

type Inp a = a
type Out a = a
type Sta a = a
