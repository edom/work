{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Unsafe #-}

{- |
Exporting the constructor shall make this module unsafe.
-}
module Sound.Buffer
(
    -- * Buffer
    Buffer(..)
    , bufUnsafePeek
    , allocaBuffer
    , withForeignBuffer
    , _bufCast
    , bufCast
    , bufForeignCast
    , bufElemType
    , bufElemSize
)
where

import Sound.Ptr

{- |

Array.

[@p@]
Could be either 'Ptr' or 'ForeignPtr'.

-}
data Buffer p a
    = MkBuffer
    {
        _bp :: !(p a)               -- ^ backing store
        , _bs :: !(ElemCount a)     -- ^ size
        , _bc :: !(ElemCount a)     -- ^ capacity
    }
    deriving (Show)

{- |
Unsafe.
-}
bufUnsafePeek :: (Storable a) => Buffer Ptr a -> (Int -> IO a)
bufUnsafePeek (MkBuffer p _ _) i = peekElemOff p i
{-# INLINE bufUnsafePeek #-}

allocaBuffer :: (Storable a) => ElemCount a -> (Buffer Ptr a -> IO b) -> IO b
allocaBuffer count consume =
    allocaArray count $ \ p ->
        consume (MkBuffer p 0 count)

withForeignBuffer :: Buffer ForeignPtr a -> (Buffer Ptr a -> IO r) -> IO r
withForeignBuffer buf consume =
    withForeignPtr (_bp buf) $ \ ptr ->
        consume buf { _bp = ptr }
{-# INLINE withForeignBuffer #-}

bufElemType :: Buffer p a -> a
bufElemType = undefined
{-# INLINE bufElemType #-}

bufElemSize :: (Storable a) => Buffer p a -> Int
bufElemSize = sizeOf . bufElemType
{-# INLINE bufElemSize #-}

_bufCast :: (Storable a, Storable b) => (p a -> p b) -> Buffer p a -> Buffer p b
_bufCast !cast src@(MkBuffer p s c) =
    let
        adj x = x * bufElemSize src `div` bufElemSize dst
        p' = cast p
        s' = adj s
        c' = adj c
        dst = MkBuffer p' s' c'
    in
        dst
{-# INLINE _bufCast #-}

bufCast :: (Storable a, Storable b) => Buffer Ptr a -> Buffer Ptr b
bufCast = _bufCast castPtr
{-# INLINE bufCast #-}

bufForeignCast :: (Storable a, Storable b) => Buffer ForeignPtr a -> Buffer ForeignPtr b
bufForeignCast = _bufCast castForeignPtr
{-# INLINE bufForeignCast #-}
