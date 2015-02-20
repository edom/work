{-# LANGUAGE BangPatterns #-}
module Sound.Amplitude
(
    -- ** Decibel amplitude
    Amplitude
    , db
    , dbn
    -- ** Limiting amplitude
    , limit
    , flimit
    , clamp
    , fclamp
    -- ** Ramp streams
    , lgeos
    -- ** Contours/envelopes
    , rlexp
)
where

import Sound.InfList
import Sound.Time

type Amplitude a = Nonnegative a

{-|

This produces an amplitude multiplier
that is equivalent to the given decibel.

@
db 0 = 1
db 20 = 10
db 40 = 100
@

-}
db :: (Floating a) => a -> Amplitude a
db !x =
    let
        !r = 10 ** (x / 20)
    in
        r
{-# INLINE db #-}

{- |
@
dbn = 'db' '.' 'negate'
@

Haskell parses @db -1@ as @db - 1@
so we would have to write @db (-1)@ or @db (negate 1)@ without @dbn@.
-}
dbn :: (Floating a) => a -> Amplitude a
dbn !x =
    let
        !r = db (negate x)
    in
        r
{-# INLINE dbn #-}

{- |
The result of @clamp mi ma x@ is @x@ if and only if @x@ is between @mi@ and @ma@,
where @mi@ cannot exceed @ma@.
-}
clamp :: (Ord a) => Min a -> Max a -> a -> a
clamp !mi !ma !x =
    case x of
        _ | x < mi -> mi
        _ | x > ma -> ma
        _ -> x

{- |
@
fclamp mi ma = 'fmap' ('clamp' mi ma)
@
-}
fclamp :: (Functor f, Ord a) => Min a -> Max a -> f a -> f a
fclamp !mi !ma = fmap (clamp mi ma)

{- |
@
limit x = 'clamp' ('negate' x) x
@
-}
limit :: (Num a, Ord a) => Amplitude a -> a -> a
limit !t = clamp (negate t) t

{- |
@
flimit x = 'fclamp' ('negate' x) x
@
-}
flimit :: (Functor f, Num a, Ord a) => Amplitude a -> f a -> f a
flimit !t = fmap (limit t)

{- |

The second argument is the exponent (@m@) in the following equation:

@
x = exp (m * t)
@

where @t@ is time in seconds.

-}
rlexp :: (Floating a) => Rate Int -> a -> RL a
rlexp rate_ expo_ =
    rated rate_ $ literate (f *) 1
    where
        f = exp (expo_ / fromIntegral (_unRate rate_))
