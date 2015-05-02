{-# LANGUAGE BangPatterns #-}

-- | Failed experiments.
module Sound.IoFail
(
    -- * Convert sample type
    lconf8i2
    , rlconf8i2
    -- * Input
    -- ** Memory
    , pvreadau
    , rvreadau
    -- ** File
    , pvreadfileau
    -- * Output
    -- ** ByteString
    , bsAuBody
    , bsAuBodyC
    -- ** Handle
    , hWriteRaw
    , ehWriteRaw
    , ghWriteRaw
    , lhWriteRaw
    , vhwrite
    , writeStreamInChunks
    -- ** File
    , writeRawFile
    , vwritefileraw
    , writeAuFile
    , lWriteRawFile
    , eWriteRawFile
    -- * Other
    , lPokeArray
    , withLArray
    , gPokeArrayR
    , gPokeArrayC
)
where

import qualified Control.Monad as Cm

import qualified Data.ByteString as Bs

import qualified Data.Serialize as Se

import qualified Data.Vector.Unboxed as Vu
import qualified Data.Vector.Storable as Vs

import Sound.Endo
import Sound.Generator
import Sound.InfList
import Sound.IoPtr
import Sound.Time
import qualified Sound.Io.Au as A

{-# DEPRECATED rvreadau "use 'pvreadau'" #-}
rvreadau :: Bs.ByteString -> Rated (Vu.Vector Double)
rvreadau =
    either (error . show) id . Se.runGet format
    where
        format = do
            magic <- Se.getWord32be -- .snd
            dataOffset <- Se.getWord32be
            dataSize <- Se.getWord32be
            encoding <- Se.getWord32be
            rate_ <- Se.getWord32be
            nchan <- Se.getWord32be
            let
                sk = (fromIntegral dataOffset :: Int) - sndMinHeaderSize
            Cm.unless (magic == 0x2e736e64) $ error "wrong magic"
            Cm.unless (nchan == 1) $ error "can't handle many channels"
            Cm.unless (encoding == 7) $ error $ "expecting encoding 7 (float64); got " ++ show encoding
            Cm.unless (sk >= 0) . error $ "invalid data offset " ++ show dataOffset
            Se.skip (fromIntegral dataOffset - sndMinHeaderSize)
            fmap (rated . mkRate . fromIntegral $ rate_) $
                Vu.replicateM (fromIntegral $ dataSize `div` 8) Se.getFloat64be

{- |
Read an AU-formatted data from memory.
-}
pvreadau :: Bs.ByteString -> Either String (Precision Int Double, Vu.Vector Double)
pvreadau =
    Se.runGet format
    where
        format = do
            magic <- Se.getWord32be -- .snd
            dataOffset <- Se.getWord32be
            dataSize <- Se.getWord32be
            encoding <- Se.getWord32be
            rate_ <- Se.getWord32be
            nchan <- Se.getWord32be
            let
                minHeaderSize = 24
                numSkipAfterHeader = fromIntegral dataOffset - minHeaderSize
            Cm.unless (magic == 0x2e736e64) $ fail "wrong magic; expecting 2e 73 6e 64"
            Cm.unless (nchan == 1) $ fail "can't handle many channels; can only handle 1 channel"
            Cm.unless (encoding == 7) . fail $ "expecting encoding 7 (float64); got " ++ show encoding
            Cm.unless (numSkipAfterHeader >= 0) . fail $ "invalid data offset " ++ show dataOffset
            Se.skip numSkipAfterHeader
            body <- Vu.replicateM (fromIntegral $ dataSize `div` 8) Se.getFloat64be
            return (fromRate rate_, body)

-- | Read an AU file.
pvreadfileau :: FilePath -> IO (Precision Int Double, Vu.Vector Double)
pvreadfileau path =
    slurp path >>= ioe . pvreadau

{- |
@bsAuBody n x@ serializes the first @n@ elements of @x@
into big-endian 64-bit floats.

@
bsAuBody n x ~ bsAuBodyC n x const
@
-}
bsAuBody :: Count -> L Double -> Bs.ByteString
bsAuBody n x =
    Se.runPut $ ltakemapM_ Se.putFloat64be n x
{-# INLINE bsAuBody #-}

{- |
This is 'bsAuBody' that allows manipulating the rest of the stream.
-}
bsAuBodyC :: Count -> L Double -> (Bs.ByteString -> L Double -> a) -> a
bsAuBodyC n x c =
    -- lcfoldl n (\ m s -> m >> Se.putFloat64be s) m0 x $ \ m t -> c (Se.runPut m) t
    -- lcfoldlM n (\ m s -> m >> Se.putFloat64be s) () x $ \ m t -> c (Se.runPut m) t
    c bs tl
    where
        (tl, bs) = Se.runPutM $ lcfoldlM n (\ _ s -> Se.putFloat64be s) () x $ \ _ t -> return t
        -- m0 = return ()
{-# INLINE bsAuBodyC #-}

-- | This is used for benchmarking.
hWriteRaw :: Handle -> Count -> L Double -> IO ()
hWriteRaw handle count stream@(MkL !_ _) =
    Bs.hPut handle (Se.runPut (ltakemapM_ Se.putFloat64le count stream))
{-# INLINE hWriteRaw #-}

eWriteRawFile :: FilePath -> Count -> Endo s -> (s -> Double) -> s -> IO ()
eWriteRawFile path count step extract initium =
    withBinaryFile path WriteMode $ \ handle ->
        ehWriteRaw handle count step extract initium
{-# INLINE eWriteRawFile #-}

ehWriteRaw :: Handle -> Count -> Endo s -> (s -> Double) -> s -> IO ()
ehWriteRaw handle count step extract initium =
    Bs.hPut handle (Se.runPut (emapM__ (Se.putFloat64le . extract) step count initium))
{-# INLINE ehWriteRaw #-}

ghWriteRaw :: Handle -> Count -> G s Double -> IO ()
ghWriteRaw handle count gen =
    Bs.hPut handle (Se.runPut (gmapM_ Se.putFloat64le count gen))
{-# INLINE ghWriteRaw #-}

lhWriteRaw :: Handle -> Count -> L Double -> IO ()
lhWriteRaw handle count gen =
    Bs.hPut handle (Se.runPut (lmapM_ Se.putFloat64le count gen))
{-# INLINE lhWriteRaw #-}

vhwrite :: (Storable a) => Handle -> Vs.Vector a -> IO ()
vhwrite handle vec =
    Vs.unsafeWith vec $ \ ptr ->
        let
            nbyte = Vs.length vec * sizeOf (un ptr)
            un :: f a -> a
            un = undefined
        in
            hPutBuf handle ptr nbyte

{- |
This writes a Storable vector into a file.

The endianness follows the machine running 'vwritefile'.
-}
vwritefileraw :: (Storable a) => FilePath -> Vs.Vector a -> IO ()
vwritefileraw path vec =
    withBinaryFile path WriteMode $ \ handle ->
        vhwrite handle vec

lPokeArray :: (Storable a) => Ptr a -> Int -> L a -> IO ()
lPokeArray ptr = limapM_ (pokeElemOff ptr)
{-# INLINE lPokeArray #-}

withLArray :: (Storable a) => Int -> L a -> (Ptr a -> IO r) -> IO r
withLArray count gen consume =
    allocaArray count $ \ ptr ->
        lPokeArray ptr count gen
        >> consume ptr
{-# INLINE withLArray #-}

-- | This is used for benchmarking.
writeRawFile :: FilePath -> Count -> L Double -> IO ()
writeRawFile path count stream =
    let
        !byteCount = 8 * count
        consume handle =
            withLArray count stream $ \ !ptr ->
                hPutBuf handle ptr byteCount
    in
        withBinaryFile path WriteMode consume
{-# INLINE writeRawFile #-}

{- |
This should be the fastest way to dump some generator samples to a file.
-}
lWriteRawFile :: FilePath -> Count -> L Double -> IO ()
lWriteRawFile path count gen =
    withBinaryFile path WriteMode $ \ handle ->
        withLArray count gen $ \ ptr ->
            hPutBuf handle ptr (8 * count)
{-# INLINE lWriteRawFile #-}

writeAuFile :: FilePath -> Word32 -> Count -> L Double -> IO ()
writeAuFile path rate_ count stream =
    withBinaryFile path WriteMode $ \ handle -> do
        bshPut handle $ A.header rate_ (fromIntegral count)
        -- bshPut handle $ bsAuBody count stream
        writeStreamInChunks chunkSize handle count stream
    where
        chunkSize = 131072
{-# INLINE writeAuFile #-}

{- |
@writeStreamInChunks m h n x@ groups the first @n@ elements of @x@
into chunks of @m@ samples each,
and writes the chunks to the handle @h@.
-}
writeStreamInChunks :: ChunkSampleCount -> Handle -> Count -> L Double -> IO ()
writeStreamInChunks chunkSize handle =
    loop
    where
        loop count stream =
            if count <= 0
                then
                    return ()
                else
                    bsAuBodyC count stream $ \ chunk rest ->
                        bshPut handle chunk
                        >> loop (count - chunkSize) rest
{-# INLINE writeStreamInChunks #-}

lconf8i2 :: L Double -> L Int16
lconf8i2 = fmap conf8i2

rlconf8i2 :: RL Double -> RL Int16
rlconf8i2 = rlmap conf8i2

gPokeArrayR :: (Storable a) => Ptr a -> Int -> G s a -> IO s
gPokeArrayR ptr = gimapM_R (pokeElemOff ptr)
{-# INLINE gPokeArrayR #-}

gPokeArrayC :: (Storable a) => Ptr a -> Int -> G s a -> (G s a -> IO r) -> IO r
gPokeArrayC ptr = gimapM_C (pokeElemOff ptr)
{-# INLINE gPokeArrayC #-}