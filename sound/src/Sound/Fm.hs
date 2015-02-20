{-# LANGUAGE Rank2Types #-}
module Sound.Fm
(
    -- * Frequency modulation
    fm
    -- * Phase modulation
    , pm
    -- * Specialized
    , ltpm
    , rltpm
    , ltfm
    , gtfm
    , rltfm
)
where

import Control.Arrow

import qualified Data.Vector.Unboxed as Vu

import Sound.Generator hiding (Kleisli)
import Sound.InfList
import Sound.Int
import Sound.Hint
import Sound.Table
import Sound.Time

fm :: (Num n) => StepSize n -> (Phase n -> Out u) -> (Inp n, Sta n) -> (Out u, Sta n)
fm d w = int d >>> first w
{-# INLINE fm #-}

pm :: (Num n) => (Phase n -> Out u) -> Inp n -> Out u
pm = id
{-# INLINE pm #-}

ltfm :: (RealFrac a, Vu.Unbox a) => StepSize a -> Carrier (Tab a) -> Modulator (L a) -> L a
ltfm p carrier modulator =
    fmap f x
    where
        f = tlookup carrier
        x = lint p modulator
{-# INLINE ltfm #-}

gtfm :: (RealFrac a, Vu.Unbox a) => Precision r a -> Carrier (Tab a) -> Modulator (G s a) -> G (P s a) a
gtfm prec carrier modulator =
    gmap f x
    where
        p = _prPeriod prec
        f = tlookup carrier
        x = gint p modulator
{-# INLINE gtfm #-}

{- | Frequency modulation wavetable synthesis.
The modulator output becomes the carrier frequency.
The integral of the modulator output becomes the carrier phase.
-}
rltfm :: (RealFrac a, Vu.Unbox a) => Carrier (Tab a) -> Modulator (RL a) -> RL a
rltfm carrier modulator = rltpm carrier (rlint modulator)
{-# INLINE rltfm #-}

ltpm :: (RealFrac a, Vu.Unbox a) => Carrier (Tab a) -> Modulator (L a) -> L a
ltpm carrier = fmap (tlookup carrier)
{-# INLINE ltpm #-}

{- | Phase modulation wavetable synthesis.
The modulator output becomes the carrier phase.
-}
rltpm :: (RealFrac a, Vu.Unbox a) => Carrier (Tab a) -> Modulator (RL a) -> RL a
rltpm carrier modulator = rlmap (tlookup carrier) modulator
{-# INLINE rltpm #-}
