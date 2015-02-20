module Sound.Portaudio
(
    -- * IO type wrapper
    Pa
    , io
    , run
    -- * Opening streams
    , withDefStream
    , start
    , stop
    -- * Carefree output
    , playsrl
    -- * Output
    , write
    , writev
    , writevs
    , writel
    , writesrl
    , getWriteAvail
)
where

import Data.Int (Int16)

import qualified Foreign as F
import qualified Foreign.C as Fc

import qualified Data.Vector.Storable as Vs
import qualified Data.Vector.Unboxed as Vu

import qualified Sound.PortAudio as P

import Sound.InfList
import Sound.Hint
import Sound.StreamVector

-- | 'Pa' is 'IO' for PortAudio.
newtype Pa a = MkPa { _runPa :: IO (Either P.Error a) }

instance Functor Pa where
    fmap f = MkPa . fmap (fmap f) . _runPa
instance Monad Pa where
    return = MkPa . return . Right
    (>>=) m k =
        MkPa $ _runPa m >>= either (return . Left) (_runPa . k)

-- | Embed an action.
io :: IO a -> Pa a
io = MkPa . fmap Right

{- |
Extract the action.
Throw an 'Ioe.IOError' as soon as there is an error.
-}
run :: Pa a -> IO a
run x = ioioe $ P.withPortAudio (_runPa x)

-- | This opens a stream with default parameters and passes it to the function.
withDefStream :: (P.Stream Int16 Int16 -> Pa a) -> Pa a
withDefStream action =
    MkPa $ P.withDefaultStream
        numInChan
        numOutChan
        samrat
        (Just framPerBuf)
        mbCallback
        mbFinalCallback
        (_runPa . action)
    where
        numInChan = 0
        numOutChan = 1
        samrat = 44100
        framPerBuf = 1024
        mbCallback = Nothing
        mbFinalCallback = Nothing

-- | Use this on the stream before you write to it.
start :: P.Stream i o -> Pa ()
start = MkPa . fmap nmbei . P.startStream

-- | Use this after you no longer use the stream before you close the stream.
stop :: P.Stream i o -> Pa ()
stop = MkPa . fmap nmbei . P.stopStream

-- * Output

{- |
Write the samples to the stream.

This blocks if the number of samples exceed the number returned by 'getWriteAvail'.
-}
write :: P.Stream i o -> Fc.CULong -> F.ForeignPtr o -> Pa ()
write s n b = MkPa . fmap nmbei $ P.writeStream s n b

-- | 'write' for unboxed 'Vu.Vector'.
writev :: (Vs.Storable o, Vu.Unbox o) => P.Stream i o -> Vu.Vector o -> Pa ()
writev s =
    writevs s . Vu.convert

-- | 'write' for storable 'Vs.Vector'.
writevs :: (Vs.Storable o) => P.Stream i o -> Vs.Vector o -> Pa ()
writevs s b =
    write s (fromIntegral len) fp
    where
        (fp, len) = Vs.unsafeToForeignPtr0 b

writel :: (Vu.Unbox o, Vs.Storable o) => P.Stream i o -> Int -> L o -> Pa ()
writel s n x =
    writev s (vfroml n x)

-- | 'write' for a sliced rated stream.
writesrl :: (Vu.Unbox o, Vs.Storable o) => P.Stream i o -> SRL Int o -> Pa ()
writesrl s x =
    writev s (vfromsrl x)

-- | Open a stream, play the signal, close the stream.
playsrl :: SRL Int Int16 -> IO ()
playsrl x =
    run $
        withDefStream $ \ s -> do
            start s
            writesrl s x
            stop s

-- | Get the number of samples that can be written to the stream without blocking.
getWriteAvail :: P.Stream i o -> Pa Int
getWriteAvail = MkPa . P.writeAvailable
