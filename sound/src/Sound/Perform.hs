module Sound.Perform
(
    -- * Instrument
    Tuning
    , Instrument
    , sineInstrument
    , defaultTuning
    , perform
    -- * Composing
    , Com
    , NoteNumber
    , Duration
    , Freq
    , note
    , sqn
    , par
)
where

import Sound.Class
import Sound.Fm
import Sound.InfList
import Sound.Table
import Sound.Time
import Unstable

type NoteNumber a = a
type Duration a = a
type Freq a = a
type Tuning a b = NoteNumber a -> Freq b

{- |
This is twelve-tone equal temperament
where note number 96 is middle C tuned to 256 Hz.
-}
defaultTuning :: (Floating a) => Tuning a a
defaultTuning n = 2 ** (n / 12)

type Instrument a = Precision Int Double -> Freq (L a) -> Build (L a) (L a)
sineInstrument :: Instrument Double
sineInstrument p f =
    return (ltfm (_prPeriod p) t f)
    where
        t = tsin 12

perform :: (Floating a) => Tuning a a -> Instrument a -> Com -> Build (L a) ()
perform tun ins =
    loop
    where
        loop com =
            case com of
                MkNote n d -> brate >>= \ r -> ins (fromRate $ _unRate r) (point $ tun $ fromIntegral n) >>= blwrite_ (bbeat d)
                Seq h t -> loop h >> loop t
                _ -> return ()

-- | Musical composition.
data Com
    = MkNote (NoteNumber Int) (Duration Rational)
    | Seq Com Com
    | Par Com Com
    deriving (Read, Show)

note :: NoteNumber Int -> Duration Rational -> Com
note = MkNote

sqn :: Com -> Com -> Com
sqn = Seq

par :: Com -> Com -> Com
par = Par
