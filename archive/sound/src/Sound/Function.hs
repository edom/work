{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

module Sound.Function
(
    -- * Types
    F
    , mkf
    , unf
    , fo
    , fid
    , fcon
    , flift
    , flift2
    -- * List-like
    , fiterate
    , ffoldl
    , ffoldlM
    -- * Transcendental functions
    , fsin
    , fcos
    , fexp
)
where

import qualified Control.Arrow as Ar
import qualified Control.Category as Cc

import Control.Arrow ((***), (&&&), first, second)

import Prelude hiding (id, (.))
import Control.Category (id, (.))

{- |
We do not define the instance @(Num b) => Num ((->) a b)@
because we avoid orphan instances.

This newtype allows writing functions in pointfree fashion.

@
'unf' '$' f '+' g = \\ x -> f x '+' g x
'unf' '$' 'id' '^' 2 '+' 'id' '+' 1 = \\ x -> x '^' 2 '+' x '+' 1
'unf' '$' 'exp' 'id' = \\ x -> 'exp' x
@

What is the difference between 'exp' and 'fexp'?

@
'fexp' :: ('Floating' a) => F a a
'exp' :: ('Floating' b) => F a b -> F a b
@

@
'fexp' = 'mkf' 'exp'
'exp' f = 'mkf' ('exp' '.' f)

'unf' 'fexp' = 'exp'
'unf' ('exp' f) = 'exp' '.' 'unf' f = \\ x -> 'exp' ('unf' f x)
@
-}
newtype F a b = MkF { _unf :: a -> b }
-- | Wrap the function.
mkf :: (a -> b) -> F a b
mkf = MkF
{-# INLINE mkf #-}
{- |
If we see the type as @F a b -> a -> b@, this applies the function to the given argument.

If we see the type as @F a b -> (a -> b)@, this unwraps the newtype.

Both of them are the same due to the right-associativity of @(->)@.
-}
unf :: F a b -> (a -> b)
unf = _unf
{-# INLINE unf #-}
-- | This is specialized 'id'.
fid :: F a a
fid = mkf id
-- | This is specialized composition ('.').
fo :: F b c -> F a b -> F a c
fo f g = mkf (unf f . unf g)
fcon :: b -> F a b
fcon = mkf . const
flift :: (b -> c) -> F a b -> F a c
flift f g = mkf (f . unf g)
flift2 :: (b -> c -> d) -> F a b -> F a c -> F a d
flift2 f g h = mkf (\ r -> f (unf g r) (unf h r))
fsin :: (Floating a) => F a a
fsin = mkf sin
fcos :: (Floating a) => F a a
fcos = mkf cos
fexp :: (Floating a) => F a a
fexp = mkf exp
instance Cc.Category F where
    id = fid
    (.) = fo
instance Ar.Arrow F where
    arr = MkF
    first = MkF . first . unf
    second = MkF . second . unf
    (***) f g = MkF (unf f *** unf g)
    (&&&) f g = MkF (unf f &&& unf g)
instance (Num b) => Num (F a b) where
    (+) = flift2 (+)
    (*) = flift2 (*)
    (-) = flift2 (-)
    negate = flift negate
    abs = flift abs
    signum = flift signum
    fromInteger = fcon . fromInteger
instance (Fractional b) => Fractional (F a b) where
    (/) = flift2 (/)
    recip = flift recip
    fromRational = fcon . fromRational
instance (Floating b) => Floating (F a b) where
    pi = fcon pi
    exp = flift exp
    log = flift log
    sin = flift sin
    cos = flift cos
    asin = flift asin
    atan = flift atan
    acos = flift acos
    sinh = flift sinh
    cosh = flift cosh
    asinh = flift asinh
    atanh = flift atanh
    acosh = flift acosh

fiterate :: F a a -> a -> Int -> a
fiterate f =
    loop
    where
        loop !x !n =
            if n <= 0
                then x
                else loop (unf f x) (n - 1)
{-# INLINE fiterate #-}

ffoldlM :: (Monad m) => F e e -> (a -> e -> m a) -> e -> Int -> a -> m a
ffoldlM f r =
    loop
    where
        loop !e !n !a =
            if n <= 0
                then return a
                else r a e >>= loop (unf f e) (n - 1)
{-# INLINE ffoldlM #-}

ffoldl :: F e e -> (a -> e -> a) -> a -> e -> Int -> a
ffoldl f r =
    loop
    where
        loop !a !e !n =
            if n <= 0
                then a
                else loop (r a e) (unf f e) (n - 1)
{-# INLINE ffoldl #-}
