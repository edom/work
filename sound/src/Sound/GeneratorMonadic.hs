{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

module Sound.GeneratorMonadic
(
    module Sound.Abstract
    -- * Construction
    , gmRepeat
    , gmIterate
    , gmIterateSimple
    -- * Running
    , gmAt
    , gmAtC
    -- * Transformation
    , gmMap
    , gmMmap
    , gmScanl
    , gmMemory
    , gmMemoryM
    , gmRisingEdge
    -- * Combination
    , gmZip2
    , gmZipA2
    , gmZipM2
    , gmZipM3
    , gmAdd
    , gmMul
    , gmZipA2C
    , gmZipA3C
    , gmAddC
    , gmMulC
    -- * Integration
    , gmInt
)
where

import Control.Applicative
import qualified Control.Monad as M

import Sound.Abstract

gmRepeat :: (Applicative m) => a -> Gm a m a
gmRepeat x = MkGm x pure pure
{-# INLINE gmRepeat #-}

{- |
@
gmMap f \< x0, x1, ... \> ~ \< f x0, f x1, ... \>
@
-}
gmMap :: (Functor m) => (a -> b) -> Gm s m a -> Gm s m b
gmMap f (MkGm s e o) = MkGm s e (fmap f . o)
{-# INLINE gmMap #-}

-- | Generalized 'gmMap'.
gmMmap :: (Monad m) => (a -> m b) -> Gm s m a -> Gm s m b
gmMmap g (MkGm s e o) = MkGm s e (o M.>=> g)
{-# INLINE gmMmap #-}

{- |
@
gmZip2 k \< x0, x1, ... \> \< y0, y1, ... \> ~ \< k x0 y0, k x1 y1, ... \>
@
-}
gmZip2 :: (Applicative m) => (a -> b -> c) -> Gm s m a -> Gm t m b -> Gm (P s t) m c
gmZip2 = gmZipA2
{-# INLINE gmZip2 #-}

-- | Same as 'gmZip2'.
gmZipA2 :: (Applicative m) => (a -> b -> c) -> Gm s m a -> Gm t m b -> Gm (P s t) m c
gmZipA2 f (MkGm s0a ea oa) (MkGm s0b eb ob) =
    MkGm
        (MkP s0a s0b)
        (\ (MkP sa sb) -> MkP <$> ea sa <*> eb sb)
        (\ (MkP sa sb) -> f <$> oa sa <*> ob sb)
{-# INLINE gmZipA2 #-}

gmZipA2C :: (Applicative m) => (a -> b -> c) -> Gm s m a -> Gm t m b -> (Gm (P s t) m c -> r) -> r
gmZipA2C f (MkGm s0a ea oa) (MkGm s0b eb ob) consume =
    consume $
        MkGm
            (MkP s0a s0b)
            (\ (MkP sa sb) -> MkP <$> ea sa <*> eb sb)
            (\ (MkP sa sb) -> f <$> oa sa <*> ob sb)
{-# INLINE gmZipA2C #-}

gmZipA3C :: (Applicative m) => (a -> b -> c -> z) -> Gm s m a -> Gm t m b -> Gm u m c -> (Gm (P s (P t u)) m z -> r) -> r
gmZipA3C f (MkGm s0a ea oa) (MkGm s0b eb ob) (MkGm s0c ec oc) consume =
    consume $
        MkGm
            (MkP s0a (MkP s0b s0c))
            (\ (MkP sa (MkP sb sc)) -> MkP <$> ea sa <*> (MkP <$> eb sb <*> ec sc))
            (\ (MkP sa (MkP sb sc)) -> f <$> oa sa <*> ob sb <*> oc sc)
{-# INLINE gmZipA3C #-}

gmAddC :: (Applicative m, Num a) => Gm s m a -> Gm t m a -> (Gm (P s t) m a -> r) -> r
gmAddC = gmZipA2C (+)
{-# INLINE gmAddC #-}

gmMulC :: (Applicative m, Num a) => Gm s m a -> Gm t m a -> (Gm (P s t) m a -> r) -> r
gmMulC = gmZipA2C (*)
{-# INLINE gmMulC #-}

-- | Generalized 'gmZip2'.
gmZipM2 :: (Applicative m, Monad m) => (a -> b -> m c) -> Gm s m a -> Gm t m b -> Gm (P s t) m c
gmZipM2 k (MkGm s0a ea oa) (MkGm s0b eb ob) =
    MkGm
        (MkP s0a s0b)
        (\ (MkP sa sb) -> MkP <$> ea sa <*> eb sb)
        (\ (MkP sa sb) -> M.join (k <$> oa sa <*> ob sb))
{-# INLINE gmZipM2 #-}

gmZipM3 :: (Applicative m, Monad m) => (a -> b -> c -> m z) -> Gm s m a -> Gm t m b -> Gm u m c -> Gm (P s (P t u)) m z
gmZipM3 k (MkGm s0a ea oa) (MkGm s0b eb ob) (MkGm s0c ec oc) =
    let
        s0z = MkP s0a (MkP s0b s0c)
        ez (MkP sa (MkP sb sc)) = (\ a b c -> MkP a (MkP b c)) <$> ea sa <*> eb sb <*> ec sc
        oz (MkP sa (MkP sb sc)) = M.join (k <$> oa sa <*> ob sb <*> oc sc)
    in
        MkGm s0z ez oz
{-# INLINE gmZipM3 #-}

{- |
@
gmAdd \< x0, x1, ... \> \< y0, y1, ... \> ~ \< x0 + y0, x1 + y1, ... \>
@
-}
gmAdd :: (Applicative m, Num a) => Gm s m a -> Gm t m a -> Gm (P s t) m a
gmAdd = gmZipA2 (+)
{-# INLINE gmAdd #-}

{- |
@
gmMul \< x0, x1, ... \> \< y0, y1, ... \> ~ \< x0 * y0, x1 * y1, ... \>
@
-}
gmMul :: (Applicative m, Num a) => Gm s m a -> Gm t m a -> Gm (P s t) m a
gmMul = gmZipA2 (*)
{-# INLINE gmMul #-}

infixl 5 `gmMul`
infixl 4 `gmAdd`

{- |
@
gmIterate k x ~ \< x, k x, k (k x), k (k (k x)), ... \>
@
-}
gmIterate :: (Monad m) => (a -> m a) -> a -> Gm a m a
gmIterate !k !x = MkGm x k return
{-# INLINE gmIterate #-}

gmIterateSimple :: (Monad m) => (a -> a) -> a -> Gm a m a
gmIterateSimple !f = gmIterate k
    where
        k !x = return $! f x
{-# INLINE gmIterateSimple #-}

gmInt :: (Applicative m, Num a) => a -> Gm s m a -> Gm (P a s) m a
gmInt !dx (MkGm s0 e0 o0) = MkGm s1 e1 o1
    where
        !s1 = MkP 0 s0
        {-
        -- If Monad m instead of Applicative m:
        e1 (MkP y s) = do
            !fx <- o0 s
            !s' <- e0 s
            let !y' = y + fx * dx
            return $! MkP y' s'
        -}
        e1 (MkP y s) = (\ !fx !s' -> MkP (y + fx * dx) s') <$> o0 s <*> e0 s
        o1 (MkP a _) = pure a
{-# INLINE gmInt #-}

gmAt :: (Monad m) => Gm s m a -> Int -> m a
gmAt (MkGm s_ e o) !n_ =
    loop n_ s_
    where
        loop !n !s =
            -- GHC somehow thinks this is not strict enough on s.
            if n > 0
                then e s >>= loop (n - 1)
                else o s
{-# INLINE gmAt #-}

gmAtC :: (Monad m) => Gm s m a -> Int -> (a -> m r) -> m r
gmAtC (MkGm s_ e o) n_ co =
    loop n_ s_
    where
        loop n s =
            if n > 0
                then e s >>= loop (n - 1)
                else o s >>= co
{-# INLINE gmAtC #-}

-- | A generator that remembers its last output.
gmMemory :: (Monad m) => (Previous a -> a -> b) -> Previous a -> Gm s m a -> Gm (P (Previous a) s) m b
gmMemory !comb !initium (MkGm s e o) = MkGm s_ e_ o_
    where
        s_ = MkP initium s
        e_ (MkP _ u) = do
            !a' <- o u
            !u' <- e u
            return $! MkP a' u'
        o_ (MkP a u) = do
            !a' <- o u
            return $! comb a a'
{-# INLINE gmMemory #-}

gmMemoryM :: (Monad m) => (Previous a -> a -> m b) -> Previous a -> Gm s m a -> Gm (P (Previous a) s) m b
gmMemoryM !comb !initium (MkGm s e o) = MkGm s_ e_ o_
    where
        s_ = MkP initium s
        e_ (MkP !_ u) = do
            !a' <- o u
            !u' <- e u
            return $! MkP a' u'
        o_ (MkP a u) = do
            !a' <- o u
            comb a a'
{-# INLINE gmMemoryM #-}

gmRisingEdge :: (Monad m) => Gm s m IntBool -> Gm (P (Previous IntBool) s) m IntBool
gmRisingEdge = gmMemory comb ibFalse
    where
        comb x y =
            case MkP x y of
                MkP 0 0 -> ibFalse
                MkP 0 _ -> ibTrue
                MkP _ _ -> ibFalse
{-# INLINE gmRisingEdge #-}

gmScanl :: (Monad m) => (a -> e -> m a) -> a -> Gm s m e -> Gm (P s a) m a
gmScanl !r !a_ (MkGm s e o) = MkGm s_ e_ o_
    where
        !s_ = MkP s a_
        e_ (MkP u a) = do
            !x <- o u
            !s' <- e u
            !a' <- r a x
            return $! MkP s' a'
        o_ (MkP !_ x) = return $! x
{-# INLINE gmScanl #-}
