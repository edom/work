{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Unsafe #-}

{- |
Exporting the constructor shall make this module unsafe.
-}
module Sound.Buffer
(
    -- * Buffer
    Buffer(..)
    -- * Properties
    , bufPtr
    , bufSiz
    , bufCap
    , bufSizeBytes
    -- * Creation
    , allocaBuffer
    , bufWithForeignPtr
    , withForeignBuffer
    -- * Casting
    , _bufCast
    , bufCast
    , bufForeignCast
    -- * Storable.sizeOf helpers
    , bufElemType
    , bufElemSize
)
where

import Sound.Ptr

{- |
A buffer is an array.

[@p@]
Either 'Ptr' or 'ForeignPtr'.

[@a@]
Element type.
-}
data Buffer p a
    = MkBuffer
    {
        _bp :: !(p a)               -- ^ backing store
        , _bs :: !(ElemCount a)     -- ^ size
        , _bc :: !(ElemCount a)     -- ^ capacity
    }
    deriving (Show)

bufPtr :: Buffer p a -> p a
bufPtr = _bp

bufSiz :: Buffer p a -> ElemCount a
bufSiz = _bs

bufCap :: Buffer p a -> ElemCount a
bufCap = _bc

_bufElemType :: Buffer p a -> a
_bufElemType = undefined

bufElemSizeBytes :: (Storable a) => Buffer p a -> Int
bufElemSizeBytes = sizeOf . _bufElemType

bufSizeBytes :: (Storable a) => Buffer p a -> Int
bufSizeBytes b = _bs b * bufElemSizeBytes b

instance (PeekElemOff p a) => PeekElemOff (Buffer p) a where
    peekElemOff (MkBuffer p _ _) i = peekElemOff p i
    {-# INLINE peekElemOff #-}

allocaBuffer :: (Storable a) => ElemCount a -> (Buffer Ptr a -> IO b) -> IO b
allocaBuffer count consume =
    allocaArray count $ \ p ->
        consume (MkBuffer p 0 count)

bufWithForeignPtr :: ElemCount e -> ElemCount e -> ForeignPtr e -> (Buffer Ptr e -> IO r) -> IO r
bufWithForeignPtr siz cap fpt act = do
    withForeignPtr fpt $ \ ptr -> do
        act $ MkBuffer ptr siz cap

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
