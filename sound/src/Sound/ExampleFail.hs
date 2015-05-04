module Sound.ExampleFail
(
    module Sound
    , f
    , g
    , rd
    , vu
    , trymidi
    , silence
    , silenceRawFile
    , silenceFile
    , eSilence
    , eSilenceRawFile
    , sineSweep
    , sineSweepRawFile
    , srl
    , trypa
    , main
)
where

import Prelude ()

import qualified Prelude as P

import qualified Data.Vector.Unboxed as Vu

import Sound
import qualified Sound.Portaudio as Pa

-- | All zeroes.
silence :: L Double
silence = point 0
{-# INLINE silence #-}

eSilence :: Endo Double
eSilence _ = 0

-- | Write 'silence' to @/tmp/silence.raw@.
silenceRawFile :: IO ()
silenceRawFile =
    lWriteRawFile path nsam silence
    where
        prec = fromRate (44100 :: Int) :: Precision Int Double
        path = "/tmp/silence.raw"
        nsam = round (secondToSample prec 10 :: Double)

eSilenceRawFile :: IO ()
eSilenceRawFile =
    eWriteRawFile path nsam eSilence extract initium
    where
        prec = fromRate (44100 :: Int) :: Precision Int Double
        path = "/tmp/silence.raw"
        nsam = round (secondToSample prec 10 :: Double)
        extract = id
        initium = 0

-- | Write 'silence' to @/tmp/silence.au@.
silenceFile :: IO ()
silenceFile =
    writeAuFile path rate_ nsam silence
    where
        prec = fromRate (44100 :: Int) :: Precision Int Double
        rate_ = fromIntegral $ _prRate prec
        path = "/tmp/silence.au"
        nsam = round (secondToSample prec 10 :: Double)

-- | 10-second exponential chirp from 20 to 20,000 Hz.
sineSweep :: StepSize Double -> L Double
sineSweep per =
    sfm per tab fre
    where
        tab = tsin 12
        fre = urampexp per 0 20 10 20000
{-# INLINE sineSweep #-}

sineSweepRawFile :: IO ()
sineSweepRawFile =
    writeRawFile path nsam (sineSweep peri)
    where
        prec = fromRate (44100 :: Int)
        peri = _prPeriod prec
        path = "/tmp/sinesweep.raw"
        nsam = round $ (secondToSample prec 10 :: Double)

testsig :: L Double
testsig =
    sfm (_prPeriod p) t j
    where
        t = tsin 12
        p = fromRate (44100 :: Int)
        d = round (secondToSample p 0.5 :: Double)
        h = ltakeappend d
        j =
            ($ 100)
            $
                    h 1000 <<< h 5000
                <<< h 1000 <<< h 5100
                <<< h 1000 <<< h 5200
                <<< h 1000 <<< h 5300
                <<< h 1000 <<< h 5400
                <<< h 1000 <<< h 5500
                <<< h 1000 <<< h 5600
                <<< h 1000 <<< h 5700
                <<< h 1000 <<< h 5800
                <<< h 1000 <<< h 5900
                <<< h 1000 <<< h 6000
                <<< h 1000 <<< h 6100
                <<< h 1000 <<< h 6200
                <<< h 1000 <<< h 6300
                <<< h 1000 <<< h 6400
                <<< h 1000 <<< h 6500
                <<< h 1000 <<< h 6600
                <<< h 1000 <<< h 6700
                <<< h 1000 <<< h 6800
                <<< h 1000 <<< h 6900

trypa :: IO ()
trypa = do
    Pa.withDefStream $ \ stm -> do
        Pa.start stm
        _ <- Pa.getWriteAvail stm
        Pa.write stm $ MkTakeF (round (secondToSample p 2 :: Double)) (convert <$> testsig)
        Pa.stop stm
    return ()
    where
        p :: Precision Int Double
        p = fromRate (44100 :: Int)

rd :: Tab Double
rd = trandoms 12 $ mkStdGen 0

vu :: Vu.Vector Double
vu = vconvolve (vfromlist [0,1,1]) (vfromlist [1,2,3,4,5])

trymidi :: IO (Either String Smf)
trymidi = do
    bs <- slurp "rosegarden.mid"
    return $ readsmf bs

{-
foo :: IO ()
foo = do
    -- sliswritefileau "/tmp/test.au" exslg
    -- srlwritefileau "/tmp/test.au" exsrl
    k <- rlreadfilewav "/home/erik/drum-samples/Samples/kick-acoustic01.wav"
    s <- rlreadfilewav "/home/erik/drum-samples/Samples/snare-acoustic01.wav"
    let secs = map (realToFrac :: Rational -> Double) $ take 512 $ listfroml $
            applytempo (rlbpm $ rlramplin (mkRate 16) 0 120 10 480) lnat
        durs = zipWith (-) (tail secs) secs
        patches = cycle [k, s]
        foobe = foldr (\ (d,p) s_ -> rltakeappends d p s_) (rlrepeat 0) (zip durs patches)
    print secs
    srlwritefileau "/tmp/test.au" $ rltake (10 * 44100) foobe
-}

srl :: IO ()
srl =
    writeAuFile "/tmp/test.au" r d testsig
    where
        p :: Precision Int Double
        p = fromRate (44100 :: Int)
        r = fromIntegral $ _prRate p
        d = secondToSample p 20

f :: Double -> Double
f t =
    exp (m * t + n)
    where
        f0 = 20
        f1 = 20000
        t0 = 0
        t1 = 60
        m = (log f1 - log f0) / (t1 - t0)
        n = log f0 - m * t0
       -- f = exp (m * t + n)
       -- m * t + n = log f

g :: Double -> Double
g t =
    exp (m * t + n)
    where
        f0 = 6000
        f1 = 14000
        t0 = 0
        t1 = 60
        m = (log f1 - log f0) / (t1 - t0)
        n = log f0 - m * t0
       -- f = exp (m * t + n)
       -- m * t + n = log f

main :: IO ()
main = putStrLn "Hello."
