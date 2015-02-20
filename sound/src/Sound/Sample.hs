{-# LANGUAGE BangPatterns #-}
module Sound.Sample
(
    lsample
    , gsample
)
where

import Sound.Generator
import Sound.Hint
import Sound.InfList

{- |
Evaluate a function at each time
that is an integral multiple of the sampling period.
-}
lsample :: (Num t) => StepSize t -> (t -> a) -> L a
lsample !p !f =
    lunfoldr (p +) f 0
{-# INLINE lsample #-}

gsample :: (Num t) => StepSize t -> (t -> a) -> G t a
gsample !p !f =
    MkG 0 (p +) f
{-# INLINE gsample #-}
