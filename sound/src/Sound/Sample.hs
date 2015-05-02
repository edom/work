{-# LANGUAGE BangPatterns #-}
module Sound.Sample
(
    sample
)
where

import Sound.Class
import Sound.Hint

{- |
Evaluate a function at each time
that is an integral multiple of the sampling period.
-}
sample :: (Unfold f, Num t) => StepSize t -> (t -> a) -> f a
sample p f = unfold f (p +) 0
{-# INLINE sample #-}
