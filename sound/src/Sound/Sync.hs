{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

module Sound.Sync
(
    module Sound.Abstract
    , Master
    , Slave
    , gHardSync
    , gmHardSync
    , gmHardSyncIb
    , lhardsyncbool
    , lhardsync
)
where

import Control.Applicative

import Sound.Abstract
import Sound.InfList

{- |
If the master produces true, the slave is reset.

Each 'True' in the master coincides with the head of the slave in the result.

Example:

@
master  F   F   T   F   F   T   T   F   F   F
slave   0   1   2   3   4   5   6   7   8   9
result  0   1   0   1   2   0   0   1   2   3
@

This may cause aliasing.
-}
gHardSync :: Master (G s Bool) -> Slave (G t a) -> G (P s t) a
gHardSync (MkG s0 e0 o0) (MkG s1 e1 o1) =
    MkG s e o
    where
        s = MkP s0 s1
        e (MkP u v) =
            let
                !b = o0 u
                !u' = e0 u
                !v' = e1 v
            in
                MkP u' $! if b
                    then s1
                    else v'
        o (MkP _ v) = o1 v
{-# INLINE gHardSync #-}

gmHardSyncIb :: (Applicative m) => Master (Gm s m IntBool) -> Slave (Gm t m a) -> Gm (P s t) m a
gmHardSyncIb (MkGm s0 e0 o0) (MkGm s1 e1 o1) =
    MkGm s_ e_ o_
    where
        s_ = MkP s0 s1
        e_ (MkP u v) = (\ !b !u' !v' -> MkP u' $! ibIf b s1 v') <$> o0 u <*> e0 u <*> e1 v
        o_ (MkP _ v) = o1 v
{-# INLINE gmHardSyncIb #-}

gmHardSync :: (Applicative m) => Master (Gm s m Bool) -> Slave (Gm t m a) -> Gm (P s t) m a
gmHardSync (MkGm s0 e0 o0) (MkGm s1 e1 o1) =
    MkGm s_ e_ o_
    where
        s_ = MkP s0 s1
        e_ (MkP u v) = (\ !b !u' !v' -> MkP u' $! if b then s1 else v') <$> o0 u <*> e0 u <*> e1 v
        o_ (MkP _ v) = o1 v
{-# INLINE gmHardSync #-}

lhardsyncbool :: Master (L Bool) -> Slave (L a) -> L a
lhardsyncbool rst (MkL stm0 (MkL stm1 stmt)) =
    loop rst stm0 (MkL stm1 stmt)
    where
        loop (MkL rh rt) s0 (MkL s1 st) =
            if rh
                then MkL stm0 (loop rt stm1 stmt)
                else MkL s0 (loop rt s1 st)

{- |
Reset slave when the master is rising and crosses zero.

If the previous master sample is negative
and the current master slave is nonnegative,
the output sample becomes the first slave sample.

If the master is a sine wave and the slave period is greater than the master period,
the output period is the master period.

This may cause aliasing.

Examples:

@
master  0   0   0   0   0   0   0   0
slave   0   1   2   3   4   5   6   7
result  0   1   2   3   4   5   6   7

master  0   1   0   -1  0   1   0   -1
slave   0   1   2   3   4   5   6   7
result  0   1   2   3   0   1   2   3

master  0   1   2   1   0   -1  -2  -1  0   1   2   1
slave   0   1   2   3   4   5   6   7   8   9   10  11
result  0   1   2   3   4   5   6   7   0   1   2   3

master  1   -1  1   -1  1   -1  1   -1
slave   0   1   2   3   4   5   6   7
result  0   1   0   1   0   1   0   1

master  -1  1   -1  1   -1  1   -1  1
slave   0   1   2   3   4   5   6   7
result  0   0   1   0   1   0   1   0
@
-}
lhardsync :: (Num a, Ord a) => Master (L a) -> Slave (L a) -> L a
lhardsync master slave =
    ldecons master $ \ mh mt ->
    ldecons slave $ \ sh st ->
        loop mh mt sh st
    where
        -- suffixes: p = previous, c = current, n = next
        loop mp m outc s =
            ldecons m $ \ mc mn ->
                let
                    outn = 
                        if mp < 0 && mc >= 0
                            then slave
                            else s
                in
                    ldecons outn $ \ outh outt ->
                        lcons outc $ loop mc mn outh outt

-- | Something controlling a 'Slave'.
type Master a = a

-- | Something being controlled by a 'Master'.
type Slave a = a
