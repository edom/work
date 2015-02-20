{-# LANGUAGE BangPatterns #-}
module Sound.GenBody
(
    module Sound.Abstract
    -- * Construction
    , gbmPass
    , gbmRepeat
    , gbmIterate
    , gbmIterateSimple
    -- * Deconstruction
    , gbmAt
    , gbmAtManual
    , Start(..)
    -- * Combination
    , gbmZipM2
    , gbmAdd
    , gbmMul
    -- * Integral
    , gbmInt
    -- * Scan
    , gbmScanl
)
where

import Sound.Abstract

gbmPass :: (Monad m) => GenBodyM a m a
gbmPass = MkGenBodyM return return
{-# INLINE gbmPass #-}

gbmRepeat :: (Monad m) => a -> GenBodyM () m a
gbmRepeat val = MkGenBodyM return (\ _ -> return val)
{-# INLINE gbmRepeat #-}

gbmIterate :: (Monad m) => (a -> m a) -> GenBodyM a m a
gbmIterate next = MkGenBodyM next return
{-# INLINE gbmIterate #-}

gbmIterateSimple :: (Monad m) => (a -> a) -> GenBodyM a m a
gbmIterateSimple next = MkGenBodyM (return . next) return
{-# INLINE gbmIterateSimple #-}

gbmAt :: (Monad m, Start s) => GenBodyM s m a -> Int -> m a
gbmAt g = gbmAtManual g start
{-# INLINE gbmAt #-}

gbmAtManual :: (Monad m) => GenBodyM s m a -> s -> Int -> m a
gbmAtManual (MkGenBodyM e o) =
    loop
    where
        loop !s !n = do
            s' <- e s
            if n <= 0
                then o s
                else loop s' (n - 1)
{-# INLINE gbmAtManual #-}

gbmAdd :: (Monad m, Num a) => GenBodyM s m a -> GenBodyM t m a -> GenBodyM (P s t) m a
gbmAdd = gbmZipM2 (\ a b -> return $! a + b)
{-# INLINE gbmAdd #-}

gbmMul :: (Monad m, Num a) => GenBodyM s m a -> GenBodyM t m a -> GenBodyM (P s t) m a
gbmMul = gbmZipM2 (\ a b -> return $! a * b)
{-# INLINE gbmMul #-}

infixl 4 `gbmAdd`
infixl 5 `gbmMul`

gbmScanl :: (Monad m) => (a -> e -> m a) -> GenBodyM s m e -> GenBodyM (P a s) m a
gbmScanl reduce (MkGenBodyM e o) =
    MkGenBodyM e' o'
    where
        e' (MkP a s) = do
            x <- o s
            a' <- reduce a x
            s' <- e s
            return (MkP a' s')
        o' (MkP a _) = do
            return a

gbmInt :: (Monad m, Num a) => a -> GenBodyM s m a -> GenBodyM (P a s) m a
gbmInt d = gbmScanl (\ y x -> return (y + x * d))

gbmZipM2 :: (Monad m) => (a -> b -> m c) -> GenBodyM s m a -> GenBodyM t m b -> GenBodyM (P s t) m c
gbmZipM2 k (MkGenBodyM e0 o0) (MkGenBodyM e1 o1) =
    MkGenBodyM e o
    where
        e (MkP s t) = do
            s' <- e0 s
            t' <- e1 t
            return (MkP s' t')
        o (MkP s t) = do
            a <- o0 s
            b <- o1 t
            k a b
{-# INLINE gbmZipM2 #-}

{- |
Initial values for generator states.
-}
class Start a where start :: a

instance Start () where start = ()
instance Start Int where start = 0
instance Start Double where start = 0
instance (Start a, Start b) => Start (P a b) where start = MkP start start
