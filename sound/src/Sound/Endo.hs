{-# LANGUAGE BangPatterns #-}
module Sound.Endo
(
    -- * Resumable execution
    ecompose
    , elist
    -- * Generally irresumable execution
    , efoldl
    -- * Combinator
    , e0scanl
    , e0int
    , eiso
    -- -- * Frequency modulation
    -- , etfm
    -- * Monadic
    , emapM_
    , emapM__
)
where

import Sound.Abstract

{- |
This immediately consumes the produced virtual list.
-}
efoldl :: (a -> e -> a) -> Endo e -> a -> e -> Int -> a
efoldl r f =
    loop
    where
        loop !a !e !n =
            if n <= 0
                then a
                else loop (r a e) (f e) (n - 1)
{-# INLINE efoldl #-}

{- |
The first element is the accumulator.
-}
e0scanl :: (a -> e -> a) -> Endo e -> Endo (P a e)
e0scanl r f (MkP a e) = MkP (r a e) (f e)
{-# INLINE e0scanl #-}

{- |
The first element is the integral.
-}
e0int :: (Num a) => a -> Endo a -> Endo (P a a)
e0int dx f = e0scanl (\ y fx -> y + fx * dx) f
{-# INLINE e0int #-}

-- etlookup :: Tab a -> Endo Int -> Endo (a, Int)
-- etlookup t f (!a, !i) = (tlookup t i, f i)

{- |
This is like 'map'.
-}
eiso :: (a -> b) -> (b -> a) -> Endo a -> Endo b
eiso ab ba aa = ab . aa . ba

{- |
@
ecompose f n = f '.' f '.' f '.' ... '.' f
@

where @f@ occurs @n@ times at the right-hand side of the equation.
-}
ecompose :: Endo a -> Int -> a -> a
ecompose !f = loop
    where
        loop !n !x =
            let
                !g = f x
            in
                if n <= 0
                    then x
                    else loop (n - 1) g
{-# INLINE ecompose #-}

elist :: Endo a -> a -> Int -> a
elist f =
    loop
    where
        loop !x !n =
            let
                !g = f x
            in
                if n <= 0
                    then x
                    else loop g (n - 1)
{-# INLINE elist #-}

emapM_ :: (Monad m) => (a -> m u) -> Endo a -> Int -> a -> m a
emapM_ k f =
    loop
    where
        loop !n !x =
            if n <= 0
                then return x
                else k x >> loop (n - 1) (f x)
{-# INLINE emapM_ #-}

emapM__ :: (Monad m) => (a -> m u) -> Endo a -> Int -> a -> m ()
emapM__ k f =
    loop
    where
        loop !n !x =
            if n <= 0
                then return ()
                else k x >> loop (n - 1) (f x)
{-# INLINE emapM__ #-}
