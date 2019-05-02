module Sound.IO
(
    -- * PortAudio output
    module Sound.Portaudio
    -- * Input from memory
    , rvreadau
    -- * Output
    -- ** File
    , rvwritefileau
    , lwritefileau
    , vwritefileraw
    -- ** Handle
    , vhwrite
    -- * Features requiring SoX
    -- ** Input from file
    , rvreadfilewav
    , rlreadfilewav
    -- ** Format conversion
    , conwavau
    -- * General strict byte-string input
    , slurp
    -- * Convert sample type
    , conf8i2
    , lconf8i2
    , rlconf8i2
    -- * Types
    , Count
)
where

import qualified Control.Monad as Cm
import qualified System.Exit as Ex
import qualified System.IO as Io
import qualified System.IO.Error as Ioe

import qualified Data.ByteString as Bs

import qualified Data.Serialize as Se

import qualified Data.Vector.Unboxed as Vu

import qualified System.Process as P

import Data.Int (Int16)

import Internal
import Sound.Portaudio
import Sound.Sample
import Sound.Stream

rvreadfilewav :: FilePath -> IO (Rated (Vu.Vector Double))
rvreadfilewav path =
    slurp path
    >>= conwavau
    >>= return . rvreadau

rlreadfilewav :: FilePath -> IO (Rated (L Double))
rlreadfilewav = fmap (rlfromrv 0) . rvreadfilewav

-- | Read the entire file into memory.
slurp :: FilePath -> IO Bs.ByteString
slurp = Bs.readFile

{- |
Convert WAV to AU.

This may not work correctly on Windows.
This may not work correctly on Haskell implementations other than GHC.
(This may hang.)

This requires <http://sox.sourceforge.net/ sox> to be in PATH.
You can install it on Ubuntu 12.04 like this:

@
sudo apt-get install sox
@
-}
conwavau :: Bs.ByteString -> IO Bs.ByteString
conwavau wav_ = do
    (Just i, Just o, _, p) <- P.createProcess (P.proc sox args)
        {
            P.cwd = Just "/tmp"
            , P.std_in = P.CreatePipe
            , P.std_out = P.CreatePipe
            , P.std_err = P.Inherit
        }
    let
        loop wav au = do
            wavTail <- Bs.hPutNonBlocking i wav
            auTail <- Bs.hGetNonBlocking o 1048576
            me <- P.getProcessExitCode p
            case me of
                Just Ex.ExitSuccess -> return au
                Just e -> Ioe.ioError . Ioe.userError $ show e
                _ -> loop wavTail (Bs.append au auTail)
    loop wav_ Bs.empty
    where
        sox = "sox"
        args = ["-", "--type", "au", "--encoding", "floating-point", "--bits", "64", "-"]

{- |
This only supports one channel of 64-bit float samples.
-}
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
                minHeaderSize = 24
                sk = fromIntegral dataOffset - minHeaderSize
            Cm.unless (magic == 0x2e736e64) $ error "wrong magic"
            Cm.unless (nchan == 1) $ error "can't handle many channels"
            Cm.unless (encoding == 7) $ error $ "expecting encoding 7 (float64); got " ++ show encoding
            Cm.unless (sk >= 0) . error $ "invalid data offset " ++ show dataOffset
            Se.skip (fromIntegral dataOffset - minHeaderSize)
            fmap (rated . mkRate . fromIntegral $ rate_) $
                Vu.replicateM (fromIntegral $ dataSize `div` 8) Se.getFloat64be

-- | Convert a 'Double' (8-byte float) sample to a 'Int16' (2-byte int) sample.
conf8i2 :: Double -> Int16
conf8i2 = truncate . (32767 *)

lconf8i2 :: L Double -> L Int16
lconf8i2 = lmap conf8i2

rlconf8i2 :: RL Double -> RL Int16
rlconf8i2 = rlmap conf8i2

type Count = Int

lwritefileau :: (Integral r) => FilePath -> Precision r p -> Count -> L Double -> IO ()
lwritefileau path prec count sig =
    Io.withBinaryFile path Io.WriteMode $ \ handle -> do
        Bs.hPut handle content
    where
        rt = fromIntegral $ _prRate prec
        content = Se.runPut $ do
            -- header
            mapM_ Se.putWord32be [magic, dataOffset, dataSize, encoding, rt, nchan]
            -- sound
            ltakemapM_ Se.putFloat64be count sig -- this may allocate a very large buffer
        magic = 0x2e736e64 -- .snd
        dataOffset = 24
        dataSize = 8 * fromIntegral count
        encoding = 7 -- double
        nchan = 1

-- | This may allocate a very large buffer.
rvwritefileau :: FilePath -> Rated (Vu.Vector Double) -> IO ()
rvwritefileau path rsig =
    Io.withBinaryFile path Io.WriteMode $ \ handle -> do
        Bs.hPut handle content
    where
        rate_ = _unRate $ rate rsig
        sig = unrated rsig
        content = Se.runPut $ do
            -- header
            mapM_ Se.putWord32be [magic, dataOffset, dataSize, encoding, fromIntegral rate_, nchan]
            -- sound
            Vu.mapM_ Se.putFloat64be sig -- this may allocate a very large buffer
        magic = 0x2e736e64 -- .snd
        dataOffset = 24
        -- FIXME use sizeOf (sample type) instead of hardcoding 8
        dataSize = 8 * fromIntegral (Vu.length sig)
        encoding = 7 -- double
        nchan = 1
