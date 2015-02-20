{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

module Sound.Time
(
    module Sound.Hint
    -- * Sampling frequency
    , Precision(..)
    , fromRate
    , secondToSample
    , sampleToSecond
    -- * Time
    , PhysicalTime
    , LogicalTime
    , SampleNumber
    -- * Tempo
    , gTickNumber
    , gmTickNumber
    , TickPerSample
    -- * Type hints
    , Beat
    , SamplePerSecond
    , TickPerBeat
    , Spb
    , Bpm
    -- * Unfavored sample rate
    , Rate(..)
    , Rated(..)
    , rated
    , rmap
    , rrmap
    , rlift2
    , mkRate
    , ror
    , ratedt
    , rate
    , unrated
    , T
)
where

import qualified Control.Applicative as Ca

import Sound.Generator
import Sound.GeneratorMonadic
import Sound.Hint

{- |
Time as measured in the real world.

The unit is second.
-}
type PhysicalTime a = a

{- |
One sample number corresponds to one sampling period.
-}
type SampleNumber a = a

{- |
Time in composition.

The unit is tick.

The tick is a quantum (the smallest logical time unit).
Each logical time quantity is an integral multiple of the tick.
-}
type LogicalTime a = a

{- |
-}
gTickNumber :: (Num a) => G s (TickPerSample a) -> G (P s a) (SampleNumber a)
gTickNumber = gscanl (+) 0
{-# INLINE gTickNumber #-}

gmTickNumber :: (Monad m, Num a) => Gm s m (TickPerSample a) -> Gm (P s a) m (SampleNumber a)
gmTickNumber = gmScanl (\ !a !b -> return $! a + b) 0
{-# INLINE gmTickNumber #-}

type TickPerSample a = a

-- | Temporal precision of sampling.
data Precision r p
    = MkPrecision
    {
        _prRate :: !r
        , _prPeriod :: !p
    }
    deriving (Read, Show)

{- |
Construct a 'Precision' from sampling rate,
also known as sampling frequency,
which is also the number of samples per second.
-}
fromRate :: (Integral r, Fractional p) => r -> Precision Int p
fromRate !r = MkPrecision pr pp
    where
        !pr = fromIntegral r
        !pp = recip (realToFrac r)
{-# INLINE fromRate #-}

-- | Convert number of seconds to number of samples.
secondToSample :: (Integral a, Num b) => Precision a p -> b -> b
secondToSample !x =
    let
        !r = fromIntegral (_prRate x)
    in
        (r *)
{-# INLINE secondToSample #-}

-- | Convert number of samples to number of seconds.
sampleToSecond :: (Num a) => Precision b a -> a -> a
sampleToSecond !x =
    let
        !p = _prPeriod x
    in
        (p *)
{-# INLINE sampleToSecond #-}

type TickPerBeat a = a
type SamplePerSecond a = a
type Beat a = a
type Spb a = a
type Bpm a = a

{- |
Sample rate.
This will be deprecated. Use 'Precision' instead.
-}
newtype Rate a
    = MkRate { _unRate :: Nonnegative a }
    deriving (Read, Show)

-- | Something with the given rate.
rated :: Rate Int -> a -> Rated a
rated r i = MkRated r i

-- | Each inhabitant of @Rated a@ has a rate.
data Rated a = MkRated { _rate :: Rate Int, _unrated :: a }

instance Functor Rated where
    fmap f x = x { _unrated = f (_unrated x) }

instance Ca.Applicative Rated where
    pure = MkRated (mkRate 0)
    (<*>) ff fx =
        MkRated
            (_rate ff `ror` _rate fx)
            (_unrated ff (_unrated fx))
instance (Num a) => Num (Rated a) where
    (+) = rlift2 (+)
    (*) = rlift2 (*)
    (-) = rlift2 (-)
    negate = rmap negate
    abs = rmap abs
    signum = rmap signum
    fromInteger = rated (mkRate 0) . fromInteger
instance (Fractional a) => Fractional (Rated a) where
    (/) = rlift2 (/)
    recip = rmap recip
    fromRational = rated (mkRate 0) . fromRational
instance (Floating a) => Floating (Rated a) where
    pi = rated (mkRate 0) pi
    exp = rmap exp
    log = rmap log
    sin = rmap sin
    cos = rmap cos
    asin = rmap asin
    atan = rmap atan
    acos = rmap acos
    sinh = rmap sinh
    cosh = rmap cosh
    asinh = rmap asinh
    atanh = rmap atanh
    acosh = rmap acosh

rmap :: (a -> b) -> Rated a -> Rated b
rmap f x = x { _unrated = f (_unrated x) }

-- | This is like 'rmap' but this also passes the rate to the function.
rrmap :: (Rate Int -> a -> b) -> Rated a -> Rated b
rrmap f x = x { _unrated = f (_rate x) (_unrated x) }

-- | This is specialized 'Ca.liftA2'.
rlift2 :: (a -> b -> c) -> Rated a -> Rated b -> Rated c
rlift2 f (MkRated r0 x0) (MkRated r1 x1) = MkRated (r0 `ror` r1) (f x0 x1)

mkRate :: a -> Rate a
mkRate = MkRate

-- | Combine two rates.
ror :: Rate Int -> Rate Int -> Rate Int
ror a b
    | _unRate a == 0 = b
    | _unRate b == 0 = a
    | _unRate a == _unRate b = a
    | otherwise = error $ "ror: operating two signals having different rates: " ++ show a ++ " and " ++ show b
-- This infix declaration follows that of (||) as exported by Prelude.
infixr 2 `ror`

ratedt :: (Real a, Fractional b) => Rate a -> b
ratedt (MkRate x) = recip (realToFrac x)

-- | Get the rate.
rate :: Rated a -> Rate Int
rate = _rate

-- | Extract the content.
unrated :: Rated a -> a
unrated = _unrated

-- | time in seconds
type T = Double
