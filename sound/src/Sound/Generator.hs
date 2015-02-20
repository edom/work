{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fwarn-unsafe #-}

-- | DEPRECATED: use "Sound.Stream" instead.
module Sound.Generator
(
    module Sound.Abstract
    -- * Construction
    , grepeat
    , giterate
    , gnat
    , gMemory
    -- * Deconstruction
    , gat
    -- * Transformation
    , gzip2
    -- * Num-like
    , gadd
    , gmul
    -- * Folds
    , gfoldl
    -- * Scans
    , gscanl
    -- * Integration
    , gint
    -- * Monadic
    , gmapM_
    , gimapM_
    -- * Monadic and resumable
    , gimapM_C
    , gimapM_R
    -- * Simple constant-frequency sine
    , gsin
)
where

import Sound.Abstract

giterate :: (a -> a) -> a -> G a a
giterate e s = MkG s e id
{-# INLINE giterate #-}

gnat :: (Num a) => G a a
gnat = giterate (1 +) 0
{-# INLINE gnat #-}

grepeat :: a -> G a a
grepeat x = MkG x id id
{-# INLINE grepeat #-}

gadd :: (Num a) => G s0 a -> G s1 a -> G (P s0 s1) a
gadd = gzip2 (+)
{-# INLINE gadd #-}

gmul :: (Num a) => G s0 a -> G s1 a -> G (P s0 s1) a
gmul = gzip2 (*)
{-# INLINE gmul #-}

infixl 5 `gmul`
infixl 4 `gadd`

{- |

A generator that remembers its last output.

In case of @gMemory c a g@,
if @c@ does not use its first argument,
there will be performance penalty due to GHC assuming it lazy.
There will be unboxing at the beginning of each iteration of the loop
and boxing at the end of each iteration of the loop.

-}
gMemory :: (Previous a -> a -> b) -> Previous a -> G s a -> G (P (Previous a) s) b
gMemory comb initium (MkG s e o) = MkG s_ e_ o_
    where
        s_ = MkP initium s
        e_ (MkP _ u) = MkP (o u) (e u)
        o_ (MkP a u) = comb a (o u)
{-# INLINE gMemory #-}

gat :: G s a -> Int -> a
gat (MkG s0 e o) n0 =
    loop n0 s0
    where
        loop n s =
            if n <= 0
                then o s
                else loop (n - 1) (e s)
{-# INLINE gat #-}

gscanl :: (a -> e -> a) -> a -> G s e -> G (P s a) a
gscanl r !a_ (MkG s0 nextState mapOut) = MkG s_ e_ o_
    where
        s_ = MkP s0 a_
        e_ (MkP s a) =
            let
                !x = mapOut s
                !s' = nextState s
                !a' = r a x
            in
                MkP s' a'
        o_ (MkP _ x) = x
{-# INLINE gscanl #-}

gint :: (Num a) => a -> G s a -> G (P s a) a
gint !dx = gscanl (\ !y !fx -> y + fx * dx) 0
{-# INLINE gint #-}

gfoldl :: (a -> e -> a) -> a -> Int -> G s e -> a
gfoldl !r !a_ !n_ (MkG s0 e o) =
    let
        loop !a !s !n =
            let
                !x = o s
                !s' = e s
                !a' = r a x
                !n' = n - 1
            in
                if n <= 0
                    then a
                    else loop a' s' n'
    in
        loop a_ s0 n_
{-# INLINE gfoldl #-}

gzip2 :: (a -> b -> c) -> G s0 a -> G s1 b -> G (P s0 s1) c
gzip2 !f =
    \ (MkG sx0 ex ox) (MkG sy0 ey oy) ->
        let
            !s = MkP sx0 sy0
            e (MkP sx sy) =
                let
                    !e0 = ex sx
                    !e1 = ey sy
                in
                    MkP e0 e1
            o (MkP sx sy) =
                let
                    !a0 = ox sx
                    !a1 = oy sy
                    !r = f a0 a1
                in
                    r
        in
            MkG s e o
{-# INLINE gzip2 #-}

gmapM_ :: (Monad m) => (a -> m b) -> Int -> G s a -> m ()
gmapM_ k n_ g =
    loop n_ (_gs g)
    where
        !e = _ge g
        !o = _go g
        loop !n !s =
            let
                !a = o s
                !t = e s
            in
                if n <= 0
                    then return ()
                    else k a >> loop (n - 1) t
{-# INLINE gmapM_ #-}

gimapM_R :: (Monad m) => (Int -> a -> m b) -> Int -> G s a -> m s
gimapM_R k !n (MkG s_ e_ o_) =
    loop 0 s_
    where
        loop !i !s =
            let
                !a = o_ s
                !t = e_ s
                !j = i + 1
            in
                if i >= n
                    then return s
                    else k i a >>= \ !_ -> loop j t
{-# INLINE gimapM_R #-}

gimapM_C :: (Monad m) => (Int -> a -> m b) -> Int -> G s a -> (G s a -> m r) -> m r
gimapM_C k !n g@(MkG s_ e_ o_) !c =
    loop 0 s_
    where
        loop !i !s =
            let
                !a = o_ s
                !t = e_ s
                !j = i + 1
                !g' = g { _gs = s }
                !cg' = c g'
            in
                if i >= n
                    then cg'
                    else k i a >> loop j t
{-# INLINE gimapM_C #-}

gimapM_ :: (Monad m) => (Int -> a -> m b) -> Int -> G s a -> m ()
gimapM_ k !n (MkG s_ e_ o_) =
    loop 0 s_
    where
        loop !i !s =
            let
                !a = o_ s
                !t = e_ s
            in
                if i >= n
                    then return ()
                    else k i a >> loop (i + 1) t
{-# INLINE gimapM_ #-}

gsin :: (Floating a) => a -> G a a
gsin !dt = MkG 0 (dt +) sin
{-# INLINE gsin #-}
