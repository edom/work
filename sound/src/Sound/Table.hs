{-# LANGUAGE BangPatterns #-}

-- | Wavetable synthesis.
module Sound.Table
(
    -- * Table-backed function
    tlookup
    , flookup
    -- * Common waveforms
    , tsin
    , tcos
    , tsqu
    , tsaw
    , ttri
    -- * Construction
    , Tab
    , tvec
    , tfun
    , Lgsize
    , mktab
    -- * Backing vector
    , untab
    , tnorm
    -- * List-like operations
    , tzip3
    , tsum
    , tminimum
    , tmaximum
    , tlength
    , tfromlist
    , timap
    -- * Convolution
    , vconvolve
    , tconvolve
    -- * Types
    , tmap
)
where

import qualified Data.Bits as B

import qualified Data.Vector.Unboxed as Vu

import Sound.Ramp
import Sound.StreamVector
import Sound.Time

{- |
@tlookup t@ is a function with period 1:

@
tlookup t (p + 1) = tlookup t p
@
-}
tlookup :: (RealFrac p, Vu.Unbox s) => Tab s -> Phase p -> Sample s
tlookup (MkTab tab) =
    \ !p ->
        let
            !q = truncate (p * n_) B..&. mask
        in
            Vu.unsafeIndex tab q
    where
        -- This assumes that the size of the table is a power of 2.
        !n = Vu.length tab
        !n_ = realToFrac n
        !mask = n - 1
{-# INLINE tlookup #-}

{- |
Wavetable lookup; input is phase.

@
flookup tab = 'fmap' ('tlookup' tab)
@
-}
flookup :: (RealFrac a, Vu.Unbox a, Functor f) => Tab a -> f (Phase a) -> f (Sample a)
flookup tab = fmap (tlookup tab)
{-# INLINE flookup #-}

-- | Vector whose length is a power of two.
-- A wavetable contains one cycle of a wave.
-- The number of samples in the table must be a power of 2.
newtype Tab a
    = MkTab { _untab :: Vu.Vector a }
    deriving (Read, Show)

tsum :: (Num a, Vu.Unbox a) => Tab a -> a
tsum = Vu.sum . _untab

tminimum :: (Ord a, Vu.Unbox a) => Tab a -> a
tminimum = Vu.minimum . _untab

tmaximum :: (Ord a, Vu.Unbox a) => Tab a -> a
tmaximum = Vu.maximum . _untab

tzip3 :: (Vu.Unbox a, Vu.Unbox b, Vu.Unbox c, Vu.Unbox d) => (a -> b -> c -> d) -> Tab a -> Tab b -> Tab c -> Tab d
tzip3 f p q r = mktab $ Vu.zipWith3 f (_untab p) (_untab q) (_untab r)

mktab :: (Vu.Unbox a) => Vu.Vector a -> Tab a
mktab v =
    if isPowerOfTwo $ Vu.length v
        then MkTab v
        else error "mktab: input length must be a power of two"
    where
        isPowerOfTwo x = x B..&. (x - 1) == 0

untab :: Tab a -> Vu.Vector a
untab = _untab

tmap :: (Vu.Unbox a, Vu.Unbox b) => (a -> b) -> Tab a -> Tab b
tmap f = MkTab . Vu.map f . _untab

timap :: (Vu.Unbox a, Vu.Unbox b) => (Int -> a -> b) -> Tab a -> Tab b
timap f = MkTab . Vu.imap f . _untab

tlength :: (Vu.Unbox a) => Tab a -> Int
tlength = Vu.length . _untab

tfromlist :: (Vu.Unbox a) => [a] -> Tab a
tfromlist = mktab . vfromlist

-- * Wavetable

-- | Base-2 logarithm of number of samples in a wavetable.
type Lgsize = Int

tvec :: (Vu.Unbox a) => PowerOfTwoLength (Vu.Vector a) -> Tab a
tvec = mktab

tfun_ :: (Vu.Unbox a, Fractional p) => (Phase p -> Sample a) -> Lgsize -> Tab a
tfun_ fn lgsize =
    MkTab $ Vu.generate n (\ k -> fn $ realToFrac k / realToFrac n)
    where
        n = B.shiftL 1 lgsize
{-# INLINE tfun_ #-}

-- | A wavetable generated from a function that takes a phase as its argument.
-- The phase is a number between 0 and 1, including 0, but not including 1.
tfun :: (Vu.Unbox a, Fractional p) => Lgsize -> (Phase p -> Sample a) -> Tab a
tfun = flip tfun_

-- | A table that contains one cycle of the sine wave.
tsin :: (Fractional a, Floating a, Vu.Unbox a) => Lgsize -> Tab a
tsin = tfun_ (\ p -> sin (2 * pi * p))
{-# INLINE tsin #-}

-- | A table that contains one cycle of the cosine wave.
tcos :: (Fractional a, Floating a, Vu.Unbox a) => Lgsize -> Tab a
tcos = tfun_ (\ p -> cos (2 * pi * p))

-- | Unantialiased square wave.
tsqu :: Lgsize -> Tab Double
tsqu =
    tfun_ f
    where
        f p
            | (p :: Double) < 0.5 = 1
            | otherwise = negate 1

{- |
Normalize the table so that the mean of the table is zero,
the minimum sample is -1, and the maximum sample is 1.
-}
tnorm :: (Ord a, Fractional a, Vu.Unbox a) => Tab a -> Tab a
tnorm table =
    tmap (\ x -> 2 * (x - cenmin) / (cenmax - cenmin) - 1) centered
    where
        centered = tmap (\ x -> x - mean) table
        cenmin = tminimum centered
        cenmax = tmaximum centered
        mean = tsum table / fromIntegral (tlength table)

-- | Unantialiased sawtooth wave.
tsaw :: Lgsize -> Tab Double
tsaw =
    tfun_ f
    where
        f p
            | (p :: Double) < 0.5   = ramplin 0 0 0.5 1 p
            | otherwise             = ramplin 0.5 (negate 1) 1 0 p

-- | Unantialiased triangle wave.
ttri :: Lgsize -> Tab Double
ttri =
    tfun_ f
    where
        f p
            | (p :: Double) < 0.25  = ramplin 0 0 0.25 1 p
            | p < 0.75              = ramplin 0.25 1 0.75 (negate 1) p
            | otherwise             = ramplin 0.75 (negate 1) 1 0 p

{- |
The first vector length should be odd.

The output length is the second vector length.

@
length x = m
length y = n
vconvolve x y ! k
    = ... + u (-1) * v (k - 1) + u 0 * v k + u 1 * v (k + 1) + ...
    = sum of u i * v (k + i) for i from (max ul vl) inclusive to (min uh vh) exclusive
    where
        u k = x (k + m / 2)
        v k = y k
@
-}
vconvolve :: (Num a, Vu.Unbox a) => Vu.Vector a -> Vu.Vector a -> Vu.Vector a
vconvolve x y =
    Vu.generate n $ \ k ->
        let
            vl = negate k       -- k + vl = 0
            vh = n - 1 - k      -- k + vh = n - 1
        in
            sum $ map (\ i -> u i * v (k + i)) [max ul vl .. min uh vh]
    where
        m = Vu.length x
        n = Vu.length y
        h = div m 2
        ul = negate h
        uh = m - 1 - h
        u k = Vu.unsafeIndex x (k + h)
        v k = Vu.unsafeIndex y k

tconvolve :: (Num a, Vu.Unbox a) => Vu.Vector a -> Tab a -> Tab a
tconvolve x (MkTab y) = MkTab $ vconvolve x y
