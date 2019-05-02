{-# LANGUAGE NoImplicitPrelude #-}
{- |
This only supports 1 channel 64-bit float sample.
-}
module Sound.Io.Snd
(
    -- * Handle
    writeHeader
    , writeBody
    , writeBody_
    , writeChunk
    , writeChunked
    , defaultChunkSampleCount
    -- * Types
    , Rate(..)
    , SampleCount(..)
    , ChunkSampleCount(..)
)
where

import Control.Monad
import Data.Word
import System.IO
import qualified Foreign as F

import Sound.Class
import qualified Sound.Buffer as B

newtype Rate a = MkRate a
newtype SampleCount a = MkSampleCount a

-- | The buffer must be at least 24 bytes long.
header :: Rate Word32 -> SampleCount Word32 -> F.Ptr a -> IO ()
header (MkRate rate) (MkSampleCount numSamples) ptr =
    zipWithM_ (F.pokeElemOff ptr_) indexes contents
    where
        indexes = [0..]
        contents = [magic, dataOffset, dataSize, encoding, rate, nchan]
        ptr_ = F.castPtr ptr
        magic = 0x2e736e64 -- .snd
        dataOffset = 24
        dataSize = 8 * numSamples
        encoding = 7 -- double
        nchan = 1

-- | See 'header'.
writeHeader :: Handle -> Rate Word32 -> SampleCount Word32 -> IO ()
writeHeader handle rate count =
    F.allocaBytes nbytes $ \ ptr -> do
        header rate count ptr
        hPutBuf handle ptr nbytes
    where
        nbytes = 24

-- | You must ensure @a = Double@.
writeChunk :: (Fill f, F.Storable a) => Handle -> SampleCount Int -> f a -> IO (f a)
writeChunk handle (MkSampleCount count) inits =
    B.allocaBuffer count $ \ buf -> do
        s' <- fill buf inits
        hPutBuf handle (B.bufPtr buf) (count * elemSize)
        return s'
    where
        elemSize = F.sizeOf $ (undefined :: f a -> a) inits
{-# INLINE writeChunk #-}

-- | Repeated 'writeChunk'.
writeChunked :: (Fill f, F.Storable a) => ChunkSampleCount Int -> Handle -> SampleCount Int -> f a -> IO (f a)
writeChunked (MkChunkSampleCount chunkSize) handle (MkSampleCount count) inits =
    loop inits count
    where
        loop s remain
            | remain <= 0 = return s
            | otherwise = do
                let this = min chunkSize remain
                s' <- writeChunk handle (MkSampleCount this) s
                loop s' (remain - this)
{-# INLINE writeChunked #-}

-- | 'writeChunked' with 'defaultChunkSampleCount'.
writeBody :: (Fill f, F.Storable a) => Handle -> SampleCount Int -> f a -> IO (f a)
writeBody = writeChunked defaultChunkSampleCount
{-# INLINE writeBody #-}

writeBody_ :: (Fill f, F.Storable a) => Handle -> SampleCount Int -> f a -> IO ()
writeBody_ h n s = void $ writeBody h n s
{-# INLINE writeBody_ #-}

defaultChunkSampleCount :: (Num a) => ChunkSampleCount a
defaultChunkSampleCount = MkChunkSampleCount 65536

newtype ChunkSampleCount a = MkChunkSampleCount a
