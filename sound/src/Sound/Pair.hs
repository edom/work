{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

-- | Strict pair.
module Sound.Pair
(
    P(..)
    -- * Construction
    , mkP
    -- * Deconstruction
    , pfst
    , psnd
    -- * Transformation
    , pmap0
    , pmap1
    , pmap
    -- * Currying
    , pcurry
    , puncurry
    -- * Strictness
    , pseq
)
where

{- |
Strict tuple.
-}
data P a b
    = MkP a b
    deriving (Read, Show, Eq, Ord)

infixr 1 `MkP`
infixr 1 `mkP`

mkP :: a -> b -> P a b
mkP = MkP
{-# INLINE mkP #-}

pfst :: P a b -> a
pfst (MkP x _) = x
{-# INLINE pfst #-}

psnd :: P a b -> b
psnd (MkP _ x) = x
{-# INLINE psnd #-}

pcurry :: (P a b -> c) -> (a -> b -> c)
pcurry f x y = f (MkP x y)
{-# INLINE pcurry #-}

puncurry :: (a -> b -> c) -> (P a b -> c)
puncurry !f (MkP x y) = f x y
{-# INLINE puncurry #-}

pseq :: P a b -> P a b
pseq (MkP a b) = MkP a b
{-# INLINE pseq #-}

pmap0 :: (a -> c) -> P a b -> P c b
pmap0 f (MkP a b) = MkP (f a) b
{-# INLINE pmap0 #-}

pmap1 :: (b -> c) -> P a b -> P a c
pmap1 f (MkP a b) = MkP a (f b)
{-# INLINE pmap1 #-}

pmap :: (a -> a') -> (b -> b') -> P a b -> P a' b'
pmap f g (MkP a b) = MkP (f a) (g b)
{-# INLINE pmap #-}
