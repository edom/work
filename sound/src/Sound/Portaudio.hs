{-# LANGUAGE NoImplicitPrelude #-}

module Sound.Portaudio
(
    -- * Initialization and termination
    withPortaudio
    -- * Opening streams
    , withDefStream
    , start
    , stop
    -- * Carefree output
    , playsrl
    -- * Output
    , Write(..)
    , writesrl
    , getWriteAvail
    -- * Reexports
    , P.Stream
)
where

import Foreign

import qualified Foreign as F
import qualified Foreign.C as Fc

import qualified Data.Vector.Storable as Vs
import qualified Data.Vector.Unboxed as Vu

import qualified Sound.PortAudio as P

import Sound.Buffer
import Sound.Class
import Sound.InfList
import Sound.Hint
import Sound.StreamVector

{- |
Must use this.

Extract the action.
Throw an 'IOError' as soon as there is an error.
-}
withPortaudio :: IO a -> IO a
withPortaudio = ioioe . P.withPortAudio . fmap Right

-- | This opens a stream with default parameters and passes it to the function.
withDefStream :: (P.Stream Int16 Int16 -> IO a) -> IO a
withDefStream action =
    ioioe $ P.withDefaultStream
        numInChan
        numOutChan
        samrat
        (Just framPerBuf)
        mbCallback
        mbFinalCallback
        (fmap Right . action)
    where
        numInChan = 0
        numOutChan = 1
        samrat = 44100
        framPerBuf = 1024
        mbCallback = Nothing
        mbFinalCallback = Nothing

-- | Use this on the stream before you write to it.
start :: P.Stream i o -> IO ()
start = ioioe . fmap nmbei . P.startStream

-- | Use this after you no longer use the stream before you close the stream.
stop :: P.Stream i o -> IO ()
stop = ioioe . fmap nmbei . P.stopStream

class Write o b where
    write :: P.Stream i o -> b o -> IO ()

instance (Storable o) => Write o (Buffer ForeignPtr) where
    write s b = ioioe . fmap nmbei $ P.writeStream s (fromIntegral $ _bs b) (bufPtr b)

instance (Storable o) => Write o (Buffer Ptr) where
    write s b = do
        fp <- newForeignPtr_ $ bufPtr b
        write s b { _bp = fp }

instance (Storable o, Fill f) => Write o (TakeF f) where
    write s (MkTakeF n x) =
        allocaBuffer n $ \ buf -> do
            _ <- fill buf x
            write s buf { _bs = n }

instance (Storable o) => Write o Vs.Vector where
    write s b =
        writeRaw s (fromIntegral len) fp
        where
            (fp, len) = Vs.unsafeToForeignPtr0 b

instance (Storable o, Vu.Unbox o) => Write o Vu.Vector where
    write s = write s . utos
        where
            utos :: (Storable a, Vu.Unbox a) => Vu.Vector a -> Vs.Vector a
            utos = Vu.convert

{- |
Write the samples to the stream.

This blocks if the number of samples exceed the number returned by 'getWriteAvail'.
-}
writeRaw :: P.Stream i o -> Fc.CULong -> F.ForeignPtr o -> IO ()
writeRaw s n b = ioioe . fmap nmbei $ P.writeStream s n b

-- | 'write' for a sliced rated stream.
writesrl :: (Vu.Unbox o, Vs.Storable o) => P.Stream i o -> SRL Int o -> IO ()
writesrl s x = write s $ unboxed $ vfromsrl x
    where
        unboxed :: Vu.Vector a -> Vu.Vector a
        unboxed = id

-- | Open a stream, play the signal, close the stream.
playsrl :: SRL Int Int16 -> IO ()
playsrl x =
    withDefStream $ \ s -> do
        start s
        writesrl s x
        stop s

-- | Get the number of samples that can be written to the stream without blocking.
getWriteAvail :: P.Stream i o -> IO Int
getWriteAvail = ioioe . P.writeAvailable
