module Sound.Example
(
    module Sound
    , silence
    , sine
    , sweep
    , whiteNoise
)
where

import Prelude ()

import Control.Arrow
import Control.Monad
import Sound
import qualified Sound.Io.Snd as S

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
        tab = tsin 4
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
