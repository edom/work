{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sound.Random
(
    -- * Randoms
    Randoms(..)
    -- * Samples
    , randomSamples
    -- * Vectors
    , vrandom
    , vrandoms
    -- * Tables
    , trandoms
    -- * Spectrums
    , vwhitespectrum
    , twhitespectrum
    -- * Reexports
    , Ran.StdGen
    , Ran.mkStdGen
)
where

import qualified Prelude as P
import qualified Data.Bits as B
import qualified Data.Complex as Cp

import qualified System.Random as Ran

import qualified Data.Vector.Unboxed as Vu

import Sound.Class
import Sound.StreamVector
import Sound.Table

class Randoms g v f where
    randoms :: g -> f v
    randomRs :: v -> v -> g -> f v

instance (Ran.Random a, FromList f) => Randoms Ran.StdGen a f where
    randoms g = fromList (error "L.randoms: list must be infinite") $ Ran.randoms g
    randomRs r s g = fromList (error "L.randoms: list must be infinite") $ Ran.randomRs (r,s) g

-- | Random sample values (random numbers between -1 and 1).
randomSamples :: (Randoms g v f, Functor f, Fractional v) => g -> f v
randomSamples = fmap (\ x -> x + x - 1) . randoms

vrandom :: forall g a. (Randoms g a [], Ran.Random a, Vu.Unbox a) => Int -> Ran.StdGen -> Vu.Vector a
vrandom size gen = vfromlist $ take size (randoms gen :: [a])

vrandoms :: forall g a. (Randoms g a [], Ran.Random a, Fractional a, Vu.Unbox a) => Int -> g -> Vu.Vector a
vrandoms size gen = Vu.map (\ x -> x + x - 1) . vfromlist $ take size (randoms gen :: [a])

trandoms :: forall g a. (Randoms g a [], Ran.Random a, Fractional a, Vu.Unbox a) => Lgsize -> g -> Tab a
trandoms lgsize gen =
    mktab $ Vu.fromList $ take n (randomSamples gen :: [a])
    where
        n = B.shiftL 1 lgsize

-- | The size must be even.
vwhitespectrum :: (Ran.Random a, RealFloat a, Vu.Unbox a) => Int -> Ran.StdGen -> Vu.Vector (Cp.Complex a)
vwhitespectrum n gen =
    vfromlist $ 0 : map (\ x -> Cp.cis (2 * pi * x)) (P.tail phases ++ reverse phases)
    where
        -- x k = x ((n - k) mod n); k = 0, 1, 2, ... n - 1
        phases = take h $ Ran.randoms gen
        h = div n 2

twhitespectrum :: (Ran.Random a, RealFloat a, Vu.Unbox a) => Lgsize -> Ran.StdGen -> Tab (Cp.Complex a)
twhitespectrum lgsize gen =
    mktab $ vwhitespectrum size gen
    where
        size = B.shiftL 1 lgsize
