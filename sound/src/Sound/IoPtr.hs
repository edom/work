{-# LANGUAGE BangPatterns #-}

{- |
On little-endian machines, IO functions only support little-endian SND format with 1 channel and 64-bit float samples.
-}
module Sound.IoPtr
(
    module Sound.Io
    , module Sound.Ptr
    , module Sound.Buffer
    -- * Array-poking
    , ePokeArray
    , gPokeArray
    -- * Buffer-dumping
    , withGArray
    -- * SND header
    , SndHeader(..)
    , sndMagic
    , sndMinHeaderSize
    , sndMaxGapSize
    , pokeSndHeader
    , peekSndHeader
    , hGetSndHeader
    -- * Handle
    , ghChunkedWrite
    , gmhChunkedWrite
    -- * File
    , gWriteRawFileOneShot
    , gWriteRawFile
    , gmWriteRawFile
    , hMustGetBuf
    , bufReadFile
    , bufWithFile
    , ptrReadFile
    , ptrWithReadFile
    , ptrWithReadRawFile
)
where

import Sound.Abstract
import Sound.Buffer
import Sound.Generator
import Sound.Io
import Sound.Ptr

sndMagic :: Word32
sndMagic = 0x2e736e64

sndMinHeaderSize :: (Num a) => a
sndMinHeaderSize = 24

sndMaxGapSize :: (Num a) => a
sndMaxGapSize = 1048576

data SndHeader
    = MkSndHeader
    {
        _shDataOffset :: !Word32
        , _shSampleCount :: !Word32
        , _shEncoding :: !Word32
        , _shRate :: !Word32
        , _shChannelCount :: !Word32
    }
    deriving (Read, Show)

pokeSndHeader :: Ptr a -> SndHeader -> IO ()
pokeSndHeader p (MkSndHeader b c d e f) = do
    expect (b == sndMinHeaderSize) $ "unsupported data offset " ++ show b
    expect (d == 7) $ "unsupported encoding " ++ show d
    expect (f == 1) $ "unsupported channel count " ++ show f
    pokeByteOff p 0 sndMagic
    pokeByteOff p 4 b
    pokeByteOff p 8 c
    pokeByteOff p 12 d
    pokeByteOff p 16 e
    pokeByteOff p 20 f

expect :: Bool -> String -> IO ()
expect cond msg = unless cond $ ioUserError msg

shGapSize :: SndHeader -> IO Int
shGapSize h = do
    expect (gapSize >= 0) "negative gap size"
    expect (gapSize <= sndMaxGapSize) "gap too large"
    return gapSize
    where
        gapSize = fromIntegral (_shDataOffset h) - sndMinHeaderSize

peekSndHeader :: Ptr a -> IO SndHeader
peekSndHeader p = do
    a <- peekByteOff p 0
    unless (a == sndMagic) $ ioUserError "wrong magic"
    MkSndHeader
        <$> peekByteOff p 4
        <*> peekByteOff p 8
        <*> peekByteOff p 12
        <*> peekByteOff p 16
        <*> peekByteOff p 20

hMustGetBuf :: Handle -> Ptr a -> Int -> IO ()
hMustGetBuf handle ptr count = do
    n <- hGetBuf handle ptr count
    unless (n == count) $ ioUserError "premature end of handle"

hGetSndHeader :: Handle -> IO SndHeader
hGetSndHeader handle =
    allocaBytes sndMinHeaderSize $ \ p -> do
        hMustGetBuf handle p sndMinHeaderSize
        header <- peekSndHeader p
        gapSize <- shGapSize header
        unless (gapSize == 0) $ allocaBytes discardBufSize $ \ discardPtr ->
            let
                loop !remaining = do
                    let !readCount = min discardBufSize remaining
                    when (readCount > 0) $ do
                        hMustGetBuf handle discardPtr readCount
                        loop (remaining - readCount)
            in
                loop gapSize
        return $! header
    where
        discardBufSize = 4096
            

ePokeArray :: (Storable a) => Endo s -> (s -> a) -> Ptr a -> Int -> s -> IO ()
ePokeArray f extract ptr count =
    loop 0
    where
        loop !i !s =
            let
                !t = f s
                !x = extract s
            in
                if i >= count
                    then return ()
                    else pokeElemOff ptr i x >> loop (i + 1) t

{- |
This should be the fastest way to write the generator samples
to the buffer with the machine endianness.

@gPokeArray p n g@ writes @n@ samples from generator @g@ to the pointer @p@.
-}
gPokeArray :: (Storable a) => Ptr a -> Int -> G s a -> IO ()
gPokeArray ptr = gimapM_ (pokeElemOff ptr)
{-# INLINE gPokeArray #-}


{- |
Write the generator samples to a buffer and pass the buffer to the consumer.
-}
withGArray :: (Storable a) => Int -> G s a -> (Ptr a -> IO r) -> IO r
withGArray !count !gen !consume =
    allocaArray count $ \ !ptr ->
        gPokeArray ptr count gen
        >> consume ptr
{-# INLINE withGArray #-}

ghChunkedWrite :: ChunkSampleCount -> Handle -> Count -> G s Double -> IO ()
ghChunkedWrite !chunkElemCount !handle !count (MkG s0 e o) =
    allocaArray chunkElemCount consume
    where
        -- GHC inliner cannot combine two loop breakers?
        -- or fail for this case?
        consume :: Ptr Double -> IO ()
        consume ptr =
            let
                loop !nu !i !s =
                    let
                        !x = o s
                        !s' = e s
                        !nb = 8 * i
                    in
                        case () of
                            _ | nu <= 0 ->
                                return ()
                            _ | i >= nu || i >= chunkElemCount ->
                                hPutBuf handle ptr nb
                                >> loop (nu - i) 0 s
                            _ ->
                                pokeElemOff ptr i x
                                >> loop nu (i + 1) s'
            in
                loop count 0 s0
{-# INLINE ghChunkedWrite #-}

gmhChunkedWrite :: ChunkSampleCount -> Handle -> Count -> Gm s IO Double -> IO ()
gmhChunkedWrite !chunkElemCount !handle !count (MkGm s0 e o) =
    allocaArray chunkElemCount consume
    where
        consume :: Ptr Double -> IO ()
        consume ptr =
            let
                loop !nu !i !s = do
                    let !nb = 8 * i
                    !x <- o s
                    !s' <- e s
                    case () of
                        _ | nu <= 0 ->
                            return ()
                        _ | i >= nu || i >= chunkElemCount ->
                            hPutBuf handle ptr nb
                            >> loop (nu - i) 0 s
                        _ ->
                            pokeElemOff ptr i x
                            >> loop nu (i + 1) s'
            in
                loop count 0 s0
{-# INLINE gmhChunkedWrite #-}

{- |
This should be the fastest way to dump some generator samples to a file.

If you run out of memory, use 'gWriteRawFile'.
-}
gWriteRawFileOneShot :: FilePath -> Count -> G s Double -> IO ()
gWriteRawFileOneShot path !count !gen =
    let
        !byteCount = 8 * count
        consume handle =
            withGArray count gen $ \ !ptr ->
                hPutBuf handle ptr byteCount
    in
        withBinaryFile path WriteMode consume
{-# INLINE gWriteRawFileOneShot #-}

gWriteRawFile :: FilePath -> Count -> G s Double -> IO ()
gWriteRawFile !path !count !stream =
    let
        consume !handle =
            ghChunkedWrite 131072 handle count stream
    in
        withBinaryFile path WriteMode consume
{-# INLINE gWriteRawFile #-}

gmWriteRawFile :: FilePath -> Count -> Gm s IO Double -> IO ()
gmWriteRawFile !path !count !stream =
    let
        consume !handle =
            gmhChunkedWrite 131072 handle count stream
    in
        withBinaryFile path WriteMode consume
{-# INLINE gmWriteRawFile #-}

bufReadFile :: FilePath -> IO (Buffer ForeignPtr Word8)
bufReadFile path = do
    size <- getFileSize path
    fptr <- mallocForeignPtrBytes size
    byteCount <- withForeignPtr fptr $ \ ptr -> ptrReadFile path ptr size
    return (MkBuffer fptr byteCount byteCount)

bufWithFile :: (Storable a) => FilePath -> (Buffer Ptr a -> IO r) -> IO r
bufWithFile path consume = do
    buf <- bufReadFile path
    withForeignBuffer buf (consume . bufCast)
{-# INLINE bufWithFile #-}

ptrReadFile :: FilePath -> Ptr a -> ByteCount -> IO ByteCount
ptrReadFile path ptr size =
    withBinaryFile path ReadMode $ \ handle -> do
        numBytesRead <- hGetBuf handle ptr size
        -- FIXME not always userError
        unless (numBytesRead == size) $ ioUserError $ path ++ " size " ++ show size ++ " read " ++ show numBytesRead
        return numBytesRead

ptrWithReadFile :: FilePath -> (ByteCount -> Ptr a -> IO r) -> IO r
ptrWithReadFile path consume = do
    size <- getFileSize path
    allocaBytes size $ \ ptr -> do
        numBytesRead <- ptrReadFile path ptr size
        consume numBytesRead ptr

ptrWithReadRawFile :: FilePath -> (ElemCount Double -> Ptr Double -> IO r) -> IO r
ptrWithReadRawFile path consume =
    ptrWithReadFile path (\ byteCount ptr -> consume (byteCount `div` 8) (castPtr ptr))
