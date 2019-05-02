{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
    -- * SND header
    , SndHeader(..)
    , sndMagic
    , sndMinHeaderSize
    , sndMaxGapSize
    , pokeSndHeader
    , peekSndHeader
    , hGetSndHeader
    -- * File
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
import Sound.Class
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

bufReadFile :: FilePath -> IO (Buffer ForeignPtr Word8)
bufReadFile path = do
    size <- getFileSize path
    fptr <- mallocForeignPtrBytes size
    byteCount <- withForeignPtr fptr $ \ ptr -> ptrReadFile path ptr size
    return (MkBuffer fptr byteCount byteCount)

bufWithFile :: (Storable a) => FilePath -> (Buffer Ptr a -> IO r) -> IO r
bufWithFile path consume_ = do
    buf <- bufReadFile path
    withForeignBuffer buf (consume_ . bufCast)
{-# INLINE bufWithFile #-}

ptrReadFile :: FilePath -> Ptr a -> ByteCount -> IO ByteCount
ptrReadFile path ptr size =
    withBinaryFile path ReadMode $ \ handle -> do
        numBytesRead <- hGetBuf handle ptr size
        -- FIXME not always userError
        unless (numBytesRead == size) $ ioUserError $ path ++ " size " ++ show size ++ " read " ++ show numBytesRead
        return numBytesRead

ptrWithReadFile :: FilePath -> (ByteCount -> Ptr a -> IO r) -> IO r
ptrWithReadFile path consume_ = do
    size <- getFileSize path
    allocaBytes size $ \ ptr -> do
        numBytesRead <- ptrReadFile path ptr size
        consume_ numBytesRead ptr

ptrWithReadRawFile :: FilePath -> (ElemCount Double -> Ptr Double -> IO r) -> IO r
ptrWithReadRawFile path consume_ =
    ptrWithReadFile path (\ byteCount ptr -> consume_ (byteCount `div` 8) (castPtr ptr))
