{- |
Fourier transforms and other related transforms.
-}
module Sound.Fourier
(
    FreqDom
    , TimeDom
    , tdivlen
    -- * DFT (Discrete Fourier transform)
    , tdft
    -- * Inverse DFT
    , tidft
    -- * Unitary DFT
    , tudft
    -- * Real-signal DFT
    , tdftr
    , tidftr
    -- * Brick-wall filters
    , tfdlp
    , ttdlp
    , ttdlpr
)
where

import qualified Data.Complex as Cp

import qualified Data.Vector.Unboxed as Vu

import Sound.StreamVector
import Sound.Table

type FreqDom a = a
type TimeDom a = a

{- |
Divide each element of the vector by the length of the vector.

Example usage:

@
tdivlen '.' 'tidft' '.' f '.' 'tdft'
@

where @f@ does something with the frequency spectrum of the input signal.
-}
tdivlen :: (Fractional a, Vu.Unbox a) => Tab a -> Tab a
tdivlen x =
    tmap (/ n) x
    where
        n = fromIntegral $ tlength x

{- |
This function transforms a time-domain signal into its frequency-domain signal
using this variant of the discrete Fourier transform:

@
y k = sum of (x k * exp (-2 * pi * i * k * m \/ n))
    for each natural m from 0 to n but not including n
    where
        n is the length of x
        x is the input of this function
        y is the output of this function
@

The input vector length must be a power of two.

This function implements
<http://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#The_radix-2_DIT_case the radix-2 decimation-in-time special case of the Cooley-Tukey discrete Fourier transform algorithm>.
-}
tdft :: (RealFloat a, Vu.Unbox a) => TimeDom (Tab (Cp.Complex a)) -> FreqDom (Tab (Cp.Complex a))
tdft input =
    mktab . _vsvector . loop $ vsvec $ untab input
    where
        loop x =
            let
                n = _vslength x
                n2 = div n 2
                xe = mkVslice 0 2 x -- 0, 2, 4, ..., n - 2
                xo = mkVslice 1 2 x -- 1, 3, 5, ..., n - 1
                ye = loop xe
                yo = loop xo
                w k = Cp.cis . negate $ 2 * pi * fromIntegral k / fromIntegral n
            in
                vsvec $
                    if n == 1
                        then
                            Vu.singleton (vshead x)
                        else
                            Vu.generate n2 (\ k -> vsUnsafeIndex ye k + w k * vsUnsafeIndex yo k) Vu.++
                            Vu.generate n2 (\ k -> vsUnsafeIndex ye k - w k * vsUnsafeIndex yo k)

{- |
This implements the following transform

@
y k = sum of (x k * exp (2 * pi * i * k * m \/ n))
@

which is @n@ times the inverse of 'tdft':

@
tidft ('tdft' x) ~ map (n *) x
'tdft' (tidft x) ~ map (n *) x
@

We implement this using
<http://en.wikipedia.org/wiki/Discrete_Fourier_transform#Expressing_the_inverse_DFT_in_terms_of_the_DFT a property of the discrete Fourier transform>.
-}
tidft :: (RealFloat a, Vu.Unbox a) => FreqDom (Tab (Cp.Complex a)) -> TimeDom (Tab (Cp.Complex a))
tidft =
    tmap Cp.conjugate . tdft . tmap Cp.conjugate

{- |
This is unitary 'tdft'.

@
tudft (tudft x) ~ x
@

-}
tudft :: (RealFloat a, Vu.Unbox a) => Tab (Cp.Complex a) -> Tab (Cp.Complex a)
tudft x =
    tmap (/ s) $ tdft x
    where
        n = fromIntegral $ tlength x
        s = sqrt n

-- | This is 'tdft' for real signals.
tdftr :: (RealFloat a, Vu.Unbox a) => TimeDom (Tab a) -> FreqDom (Tab (Cp.Complex a))
tdftr = tdft . tmap (Cp.:+ 0)

-- | This is 'tidft' for real signals.
tidftr :: (RealFloat a, Vu.Unbox a) => FreqDom (Tab (Cp.Complex a)) -> TimeDom (Tab a)
tidftr = tmap Cp.realPart . tidft

{- |
Frequency-domain brick-wall low-pass filter.

The input is a threshold and a DFT of a signal.

This keeps @thres + 1@ first elements and @thres@
last elements and replaces everything else with zero.
-}
tfdlp :: (RealFloat a, Vu.Unbox a) => Int -> FreqDom (Tab (Cp.Complex a)) -> FreqDom (Tab (Cp.Complex a))
tfdlp thres fd =
    timap (\ k x -> if k <= thres || k >= n - thres then x else 0) fd
    where
        n = tlength fd

-- | Time-domain brick-wall low-pass filter.
ttdlp :: (RealFloat a, Vu.Unbox a) => Int -> TimeDom (Tab (Cp.Complex a)) -> TimeDom (Tab (Cp.Complex a))
ttdlp thres = tdivlen . tidft . tfdlp thres . tdft

-- | Time-domain brick-wall low-pass filter for real signals.
ttdlpr :: (RealFloat a, Vu.Unbox a) => Int -> TimeDom (Tab a) -> TimeDom (Tab a)
ttdlpr thres = tdivlen . tidftr . tfdlp thres . tdftr
