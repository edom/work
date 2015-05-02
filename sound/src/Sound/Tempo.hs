module Sound.Tempo
(
    ltempof
    , ltempo
    , ltempos
    , rltempos
    , lbpm
    , rlbpm
    , applytempo
)
where

import Sound.InfList
import Sound.Int
import Sound.Hint
import Sound.Time

{- |
This transforms logical time (beat) to physical time (sample number).
-}
ltempof :: (Fractional a) => SamplePerSecond (Rate Int) -> TickPerBeat (Rate Int) -> L (Spb a) -> L (SampleNumber a)
ltempof samplepersecond tickperbeat sigtem = fmap (sps *) (ltempos tickperbeat sigtem)
    where
        sps = fromIntegral $ _unRate samplepersecond

ltempo :: SamplePerSecond (Rate Int) -> TickPerBeat (Rate Int) -> L (Spb Rational) -> L (SampleNumber Int)
ltempo sps tpb sig = fmap round (ltempof sps tpb sig)

{- |
This transforms logical time (beat) to physical time (second).

The @n@th element of @ltempos r spb@ is the time of the @n@th tick,
where @n@ begins from zero, where there are @r@ ticks in a beat.

Tick is the smallest subdivision of beat.

You can build the spb tempo signal the same way you build sound signals: using 'Build'.
-}
ltempos :: (Fractional a) => TickPerBeat (Rate Int) -> L (Spb a) -> L (Second a)
ltempos tpb_ = sint (ratedt tpb_)

rltempos :: (Fractional a) => RL (Spb a) -> RL (Second a)
rltempos = rlint

lbpm :: (Fractional a) => L (Bpm a) -> L (Spb a)
lbpm = fmap (60 /)

{- |
The input is beat-per-minute.
The output is second-per-beat.

@
spb = 60 / bpm
@
-}
rlbpm :: (Fractional a) => RL (Bpm a) -> RL (Spb a)
rlbpm = rlmap (60 /)

{- |
The beat must be a monotonically nondecreasing sequence.

Transform logical time (beats) into physical time (seconds).

Use 'Rational' for exact calculation.

The first argument is the second-per-beat signal.
The rate of this signal is the number of ticks (subdivisions of the beat).
The bigger this signal goes, the slower the music feels.
-}
applytempo :: RL (Spb Rational) -> L (Beat Rational) -> L (Second Rational)
applytempo spb =
    -- applytempo :: (Fractional a, Ord a) => RL (Spb a) -> L (Beat a) -> L (Second a)
    loop 0 (unrated $ rltempos spb)
    where
        r = _unRate $ rate spb
        loop tick ss@(MkL sh st) bs@(MkL bh bt) =
            let
                b = fromIntegral (tick :: Int) / fromIntegral r
            in
                if bh <= b
                    then MkL sh (loop tick ss bt)
                    else loop (succ tick) st bs
