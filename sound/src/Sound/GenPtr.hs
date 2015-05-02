{-# LANGUAGE BangPatterns #-}

-- | Combination of "Sound.GeneratorMonadic" and "Sound.Ptr"
module Sound.GenPtr
(
    module Sound.IoPtr
    -- * Buffer-backed generators
    , gmFromPtr
    , gmFromPtrC
    , gmFromBuffer
    , gmFromBufferC
    , gmWithBuffer
    -- * File-backed generators
    , gmRawFile
    , gmWithRawFile
)
where

import Sound.Abstract
import Sound.IoPtr

gmFromPtr :: (Storable a) => Ptr a -> ElemCount a -> Gm Int IO a
gmFromPtr ptr size =
    {-
    if size <= 0
        then error "gmFromPtr: size must be positive"
        else MkGm 0 e o
    -}
    MkGm
        0
        (\ s -> pure (if s < size - 1 then s + 1 else s))
        (peekElemOff ptr)
{-# INLINE gmFromPtr #-}

gmFromPtrC :: (Storable a) => Ptr a -> ElemCount a -> (Gm Int IO a -> IO r) -> IO r
gmFromPtrC ptr size consume =
    when (size <= 0) (ioUserError "gmFromPtrC: size must be positive")
    *> consume
        (MkGm
            0
            (\ s -> pure (if s < size - 1 then s + 1 else s))
            (peekElemOff ptr))
{-# INLINE gmFromPtrC #-}

_gmFromPtr :: (Storable a) => Ptr a -> ElemCount a -> ElemCount a -> Gm Int IO a
_gmFromPtr ptr _size maxSize =
    MkGm
        0
        (\ s -> return (if s < maxSize then s + 1 else s))
        (peekElemOff ptr)
{-# INLINE _gmFromPtr #-}

gmFromBuffer :: (Storable a) => Buffer Ptr a -> Gm Int IO a
-- gmFromBuffer (MkBuffer ptr siz _) = gmFromPtr ptr siz
gmFromBuffer (MkBuffer ptr siz _) = _gmFromPtr ptr siz (siz - 1)
{-# INLINE gmFromBuffer #-}

gmFromBufferC :: (Storable a) => Buffer Ptr a -> (Gm Int IO a -> IO r) -> IO r
gmFromBufferC (MkBuffer ptr siz _) = gmFromPtrC ptr siz
{-# INLINE gmFromBufferC #-}

gmWithBuffer :: (Storable a) => Buffer ForeignPtr a -> (Gm Int IO a -> IO r) -> IO r
gmWithBuffer (MkBuffer ptr siz _) consume =
    withForeignPtr ptr $ \ p ->
        consume (gmFromPtr p siz)

gmRawFile :: FilePath -> IO (Gm Int IO Double)
gmRawFile path = do
    b <- fmap bufForeignCast $ bufReadFile path
    let
        fptr = _bp b
        size = _bs b
        e s = return (s + 1)
        o s =
            if s < size
                then withForeignPtr fptr $ \ p -> peekElemOff p s
                else return 0
    return $! MkGm 0 e o
{-# INLINE gmRawFile #-}

{- |
The generator is only valid in the lambda.
-}
gmWithRawFile :: FilePath -> (Gm Int IO Double -> IO r) -> IO r
gmWithRawFile path consume = do
    b <- fmap bufForeignCast $ bufReadFile path
    withForeignPtr (_bp b) $ \ p ->
        let
            size = _bs b
            e s = return (s + 1)
            o s =
                if s < size
                    then peekElemOff p s
                    else return 0
        in
            consume (MkGm 0 e o)
{-# INLINE gmWithRawFile #-}
