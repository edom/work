module Sound.Ramp
(
    -- * Functions
    ramplin
    , rampexp
    -- * Streams
    , lramplin
    , lrampexp
)
where

import Sound.Hint
import Sound.InfList
import Sound.Sample

{- |
ramplin x0 y0 x1 y1 is a linear ramp that passes (x0,y0) and (x1,y1).

@f = ramplin x0 y0 x1 y1@ satisfies:

@
f x = a * x + b
f x0 = y0
f x1 = y1
@
-}
ramplin :: (Fractional a) => a -> a -> a -> a -> (a -> a)
ramplin x0 y0 x1 y1 x =
    m * x + n
    where
        m = (y1 - y0) / (x1 - x0)
        n = y0 - m * x0
{-# INLINE ramplin #-}

{- |
rampexp x0 y0 x1 y1 is an exponential ramp that passes (x0,y0) and (x1,y1).

Both y0 and y1 must be positive.

@f = rampexp x0 y0 x1 y1@ satisfies:

@
f x = exp (a * x + b)
f x0 = y0
f x1 = y1
@
-}
rampexp :: (Floating a) => a -> Positive a -> a -> Positive a -> (a -> a)
rampexp x0 y0 x1 y1 =
    exp . ramplin x0 (log y0) x1 (log y1)
{-# INLINE rampexp #-}

-- | The sampling of 'ramplin'.
lramplin :: (Fractional a) => StepSize a -> a -> a -> a -> a -> L a
lramplin p t0 v0 t1 v1 =
    lsample p (ramplin t0 v0 t1 v1)
{-# INLINE lramplin #-}

-- | The sampling of 'rampexp'.
lrampexp :: (Floating a) => StepSize a -> a -> a -> a -> a -> L a
lrampexp p t0 v0 t1 v1 =
    lsample p (rampexp t0 v0 t1 v1)
{-# INLINE lrampexp #-}
