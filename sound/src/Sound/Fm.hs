{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
module Sound.Fm
(
    -- * Frequency modulation
    -- * Arrow-style
    afm
    -- * Scan-style
    , sfm
    -- * Phase modulation
    , pm
    -- * Specialized
    , ltpm
    , rltpm
    , gtfm
    , rltfm
)
where

import Control.Arrow

import qualified Data.Vector.Unboxed as Vu

import Sound.Class
import Sound.InfList
import Sound.Int
import Sound.Hint
import Sound.Table
import Sound.Time

afm :: (Arrow a, Num n, Pair p) => StepSize n -> (Phase n -> Out u) -> a (p (Inp n) (Sta n)) (p (Out u) (Sta n))
afm d w = aint d >>^ mapFst w
{-# INLINE afm #-}

pm :: (Num n) => (Phase n -> Out u) -> Inp n -> Out u
pm = id
{-# INLINE pm #-}

{- | Frequency modulation wavetable synthesis.
The modulator output becomes the carrier frequency.
The integral of the modulator output becomes the carrier phase.
-}
sfm :: (RealFrac a, Vu.Unbox a, Functor f, Scan f) => StepSize a -> Carrier (Tab a) -> Modulator (f a) -> f a
sfm p carrier modulator =
    fmap (tlookup carrier) (sint p modulator)
{-# INLINE sfm #-}

gtfm :: (RealFrac a, Vu.Unbox a, Functor f, Integrable f) => Precision r a -> Carrier (Tab a) -> Modulator (f a) -> f a
gtfm prec carrier modulator =
    fmap f x
    where
        p = _prPeriod prec
        f = tlookup carrier
        x = int p modulator
{-# INLINE gtfm #-}

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
