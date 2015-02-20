{-# LANGUAGE BangPatterns #-}

module Sound.StreamVector
(
    vfroml
    , vfromsl
    , vfromsrl
    , vgenerate
    , vmap
    , Vslice(..)
    , mkVslice
    , vsvec
    , vshead
    , vsUnsafeIndex
    , lfromv
    , rlfromrv
    , lvappend
    , rlvappend
    , vfromlist
    , vcfromlist
    , lcyclev
)
where

import qualified Data.Complex as Cp

import qualified Data.Vector.Generic as Vg
import qualified Data.Vector.Unboxed as Vu

import Sound.InfList
import Sound.Time

lcyclev :: (Vu.Unbox a) => Int -> a -> Vu.Vector a -> L a
lcyclev cylen z =
    lcycle cylen . lfromv z

vfroml :: (Vg.Vector v a) => Int -> L a -> v a
vfroml n s = Vg.fromList $ ltakelist n s

vfromsl :: (Vg.Vector v a) => SL Int a -> v a
vfromsl s = Vg.fromList (ltakelist (_slcount s) (_unslice s))

vfromsrl :: (Vg.Vector v a) => SRL Int a -> v a
vfromsrl = vfromsl . smap unrated

vgenerate :: (Vg.Vector v a) => Int -> (Int -> a) -> v a
vgenerate = Vg.generate

vmap :: (Vu.Unbox a, Vu.Unbox b) => (a -> b) -> Vu.Vector a -> Vu.Vector b
vmap = Vu.map

data Vslice a
    = MkVslice
    {
        _vsoffset :: !Int
        , _vsstride :: !Int
        , _vslength :: !Int
        , _vsvector :: Vu.Vector a
    }
    deriving (Read, Show)
mkVslice :: Int -> Int -> Vslice a -> Vslice a
mkVslice offset stride (MkVslice o s l v) =
    MkVslice (o + offset * s) (stride * s) (l `div` stride) v

vsvec :: (Vu.Unbox a) => Vu.Vector a -> Vslice a
vsvec x = MkVslice 0 1 (Vu.length x) x

vshead :: (Vu.Unbox a) => Vslice a -> a
vshead x = Vu.unsafeIndex (_vsvector x) (_vsoffset x)

vsUnsafeIndex :: (Vu.Unbox a) => Vslice a -> Int -> a
vsUnsafeIndex s k =
    Vu.unsafeIndex (_vsvector s) (_vsoffset s + _vsstride s * k)

{-# DEPRECATED lfromv "use 'lvappend' and 'lrepeat'" #-}
lfromv :: (Vu.Unbox a) => a -> Vu.Vector a -> L a
lfromv pad vec = lvappend vec (lrepeat pad)
{-
    loop vec
    where
        loop v =
            if Vu.null v
                then lrepeat pad
                else MkL (Vu.head v) (loop (Vu.tail v))
-}

rlfromrv :: (Vu.Unbox a) => a -> Rated (Vu.Vector a) -> Rated (L a)
rlfromrv pad = rmap (lfromv pad)

lvappend :: (Vu.Unbox a) => Vu.Vector a -> L a -> L a
lvappend vec rest =
    loop vec
    where
        loop v =
            if Vu.null v
                then rest
                else MkL (Vu.head v) (loop (Vu.tail v))

rlvappend :: (Vu.Unbox a) => Vu.Vector a -> RL a -> RL a
rlvappend vec = rmap (lvappend vec)

vfromlist :: (Vg.Vector v a) => [a] -> v a
vfromlist = Vg.fromList

vcfromlist :: (RealFloat a, Vu.Unbox a) => [a] -> Vu.Vector (Cp.Complex a)
vcfromlist = vfromlist . map (Cp.:+ 0)
