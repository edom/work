module Sound.Example
(
    module Sound
    , silence
    , sine
    , sweep
    , whiteNoise
    , portaudio
)
where

import Prelude ()

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Sound
import qualified Sound.Io.Snd as S
import qualified Sound.Portaudio as Pa

-- | This writes 1048576 zero samples to \/tmp\/silence.raw.
silence :: IO ()
silence = do
    writingFile "/tmp/silence.raw" $ \ handle -> do
        S.writeBody_ handle (S.MkSampleCount 1048576) (point 0 :: Stream Double)

sine :: IO ()
sine =
    writingFile path $ \ handle -> do
        S.writeBody_ handle (S.MkSampleCount nsam) sig
    where
        sr = 44100 :: Int
        period = recip $ realToFrac sr :: Double
        path = "/tmp/sine.raw"
        nsam = 30 * sr
        sig :: Stream Double
        sig = sfm period tab fre
        tab = tsin 12
        fre = point 256

-- | This writes 30 seconds of exponential sine sweep from 20 Hz to 20000 Hz.
sweep :: IO ()
sweep =
    writingFile path $ \ handle -> do
        S.writeBody_ handle (S.MkSampleCount nsam) sig
    where
        sr = 44100 :: Int
        period = recip $ realToFrac sr :: Double
        path = "/tmp/sweep.raw"
        nsam = 30 * sr
        sig :: Stream Double
        sig = sfm period tab fre
        tab = tsin 12
        fre = urampexp period 0 20 10 20000

whiteNoise :: IO ()
whiteNoise =
    writingFile path $ \ handle -> do
        S.writeBody_ handle (S.MkSampleCount 1048576) sig
    where
        path = "/tmp/whitenoise.raw"
        gen = mkStdGen 0
        sig :: Stream Double
        sig = randomSamples gen

portaudio :: IO ()
portaudio = do
    v <- newEmptyMVar
    Pa.withPortaudio $ do
        putStrLn "press enter to terminate"
        Pa.withDefStream $ \ s -> do
            Pa.start s
            a <- async $ do
                allocaBuffer n $ \ buf_ -> do
                    let buf = buf_ { _bs = n }
                        loop_ sig = do
                            sig' <- fill buf sig
                            Pa.write s buf
                            x <- tryTakeMVar v
                            case x of
                                Nothing -> loop_ sig'
                                Just _ -> return ()
                    loop_ $ fmap convert signal
            _ <- getLine
            putMVar v $ Just ()
            wait a
            Pa.stop s
    where
        sr = 44100 :: Int
        period = recip $ realToFrac sr :: Double
        n = 256
        signal :: Stream Double
        signal = sfm period tab fre
        tab = tsin 12
        fre = point 256
