{-# LANGUAGE BangPatterns #-}

module Sound.Ptr
(
    module Data.Word
    , ElemCount
    -- * List-like Ptr
    , ptrMap
    , ptrMapM
    , ptrZip2
    , ptrZipM2
    , ptrFoldl
    , ptrFoldlM
    -- * Specialized allocation
    -- ** On the stack
    , allocaIntArray
    , allocaDoubleArray
    -- ** On the heap
    , mallocForeignPtrIntArray
    , mallocForeignPtrDoubleArray
    -- * Reexports
    -- ** Stack allocation
    , allocaArray
    , allocaBytes
    -- ** Heap allocation
    , mallocForeignPtr
    , mallocForeignPtrArray
    , mallocForeignPtrBytes
    -- ** Copying
    , copyBytes
    , moveBytes
    -- ** Types
    , castPtr
    , castForeignPtr
    , nullPtr
    , withForeignPtr
    , ForeignPtr
    , Ptr
    , Storable(..)
)
where

import Foreign
    (
        ForeignPtr
        , Ptr
        , Storable(..)
        , allocaArray
        , allocaBytes
        , castForeignPtr
        , castPtr
        , copyBytes
        , mallocForeignPtr
        , mallocForeignPtrArray
        , mallocForeignPtrBytes
        , moveBytes
        , nullPtr
        , withForeignPtr
    )

import Data.Word

type ElemCount a = Int

ptrMap :: (Storable a, Storable b) => (a -> b) -> ElemCount a -> Ptr a -> Ptr b -> IO ()
ptrMap !f !n !src !dst =
    loop 0
    where
        loop !i =
            if i < n
                then do
                    !a <- peekElemOff src i
                    let !b = f a
                    pokeElemOff dst i b
                    loop (i + 1)
                else return ()
{-# INLINE ptrMap #-}

ptrFoldl :: (Storable e) => (a -> e -> a) -> a -> ElemCount e -> Ptr e -> IO a
ptrFoldl !f !a0 !n !src =
    loop 0 a0
    where
        loop !i !a =
            if i < n
                then do
                    !e <- peekElemOff src i
                    let !a' = f a e
                    loop (i + 1) a'
                else return a
{-# INLINE ptrFoldl #-}

ptrFoldlM :: (Storable e) => (a -> e -> IO a) -> a -> ElemCount e -> Ptr e -> IO a
ptrFoldlM !k !a0 !n !src =
    loop 0 a0
    where
        loop !i !a =
            if i < n
                then do
                    !e <- peekElemOff src i
                    !a' <- k a e
                    loop (i + 1) a'
                else return a
{-# INLINE ptrFoldlM #-}

ptrZip2 :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> ElemCount a -> Ptr a -> Ptr b -> Ptr c -> IO ()
ptrZip2 !f !n !src0 !src1 !dst =
    loop 0
    where
        loop !i =
            if i < n
                then do
                    !a <- peekElemOff src0 i
                    !b <- peekElemOff src1 i
                    let !c = f a b
                    pokeElemOff dst i c
                    loop (i + 1)
                else return ()
{-# INLINE ptrZip2 #-}

ptrZipM2 :: (Storable a, Storable b, Storable c) => (a -> b -> IO c) -> ElemCount a -> Ptr a -> Ptr b -> Ptr c -> IO ()
ptrZipM2 !k !n !src0 !src1 !dst =
    loop 0
    where
        loop !i =
            if i < n
                then do
                    !a <- peekElemOff src0 i
                    !b <- peekElemOff src1 i
                    !c <- k a b
                    pokeElemOff dst i c
                    loop (i + 1)
                else return ()
{-# INLINE ptrZipM2 #-}

ptrMapM :: (Storable a, Storable b) => (a -> IO b) -> ElemCount a -> Ptr a -> Ptr b -> IO ()
ptrMapM !k !n !src !dst =
    loop 0
    where
        loop !i =
            if i < n
                then do
                    !a <- peekElemOff src i
                    !b <- k a
                    pokeElemOff dst i b
                    loop (i + 1)
                else return ()
{-# INLINE ptrMapM #-}

allocaIntArray :: ElemCount Int -> (Ptr Int -> IO a) -> IO a
allocaIntArray = allocaArray
{-# INLINE allocaIntArray #-}

allocaDoubleArray :: ElemCount Double -> (Ptr Double -> IO a) -> IO a
allocaDoubleArray = allocaArray
{-# INLINE allocaDoubleArray #-}

mallocForeignPtrIntArray :: ElemCount Int -> IO (ForeignPtr Int)
mallocForeignPtrIntArray = mallocForeignPtrArray
{-# INLINE mallocForeignPtrIntArray #-}

mallocForeignPtrDoubleArray :: ElemCount Double -> IO (ForeignPtr Double)
mallocForeignPtrDoubleArray = mallocForeignPtrArray
{-# INLINE mallocForeignPtrDoubleArray #-}
