{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
'Internal' is big so compiling it takes time;
thus we make experiments in 'Unstable'
and move them to 'Internal' after we are satisfied.
-}
module Unstable
(
    -- * Sequencing by stream-building monad
    Comonad(..)
    , mkid
    , unid
    , Build
    , buildl
    , buildrl
    , bsamno
    , brate
    , bcont
    , blint
    -- * Partial processing and resuming
    , runBuild
    , Bst
    , Iso
    , isorev
    , bimapCon
    , liftCont
    , lowerCont
    -- * Specifying duration
    , bsam
    , bsec
    , btick
    , bbeat
    , bto
    , btos
    -- * Writing output
    , blwrite
    , blwrite_
    , blwritemap
    , blwritezip
    -- * Sequencing
    -- ** Newtype-continuation sequencing
    , clmap
    , clzip
    , clpass
    , crlpass
    , crlpasss
    , crlthrus
    , crlzip
    , crlzips
    -- ** Lambda-continuation sequencing
    , rlcid
    , rlczip
)
where

import qualified Control.Monad.Trans.Cont as Mc
import qualified Control.Monad.Trans.State.Strict as Ms

import Control.Monad ((>=>))
import Control.Monad.Trans.Cont

import Data.Functor.Identity
import Sound.Class
import Sound.InfList
import Sound.Int
import Sound.Time

{-
newtype Cont r a = MkCont { _runCont :: (a -> r) -> r }
instance Functor (Cont r) where fmap f x = MkCont (\ c -> _runCont x (c . f))
instance Applicative (Cont r) where
    pure x = MkCont (\ c -> c x)
    (<*>) ff fx = MkCont (\ c -> _runCont ff (\ f -> _runCont fx (\ x -> c (f x))))
instance Monad (Cont r) where
    return x = MkCont (\ c -> c x)
    (>>=) m k = MkCont (\ c -> _runCont m (\ x -> _runCont (k x) c))
instance (Num a) => Num (Cont r a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
runCont :: Cont r a -> (a -> r) -> r
runCont (MkCont f) c = f c
{- |
@
runContId x = 'runCont' x id
@
-}
runContId :: Cont a a -> a
runContId x = runCont x id
-}

rlcid :: Int -> RL a -> (RL a -> RL a) -> RL a
rlcid n x c =
    rated r (lcid n (unrated x) (unrated . c . rated r))
    where
        r = rate x
rlczip :: Int -> (a -> b -> c) -> RL a -> RL b -> (RL a -> RL b -> RL c) -> RL c
rlczip n f x y c =
    rated r (lczip n f (unrated x) (unrated y) (\ a b -> unrated (c (rated r a) (rated r b))))
    where
        r = rate x `ror` rate y
crlzip :: Int -> (a -> b -> c) -> RL a -> RL b -> Cont (RL c) (RL a, RL b)
crlzip n f x y = cont (\ c -> rlczip n f x y (curry c))
crlzips :: T -> (a -> b -> c) -> RL a -> RL b -> Cont (RL c) (RL a, RL b)
crlzips t f x y = crlzip n f x y
    where
        r = rate x `ror` rate y
        n = round (t * fromIntegral (_unRate r))
clmap :: Int -> (a -> b) -> L a -> Cont (L b) (L a)
clmap n f x = cont (lcmap n f x)
clzip :: Int -> (a -> b -> c) -> L a -> L b -> Cont (L c) (L a, L b)
-- clzip n f x y = cont (\ c -> lczip n f x y (\ xt yt -> c (xt, yt)))
clzip n f x y = cont (\ c -> lczip n f x y (curry c))
{- |
@
clpass n = 'clmap' n 'id'
@
-}
clpass :: Int -> L a -> Cont (L a) (L a)
clpass n x = cont (lcid n x)
crlpass :: Int -> RL a -> Cont (RL a) (RL a)
crlpass n x = cont (rlcid n x)
crlpasss :: T -> RL a -> Cont (RL a) (RL a)
crlpasss t x = crlpass n x
    where
        r = rate x
        n = round (t * fromIntegral (_unRate r))
-- | This is 'crlpasss' but without three same letters in a row.
crlthrus :: T -> RL a -> Cont (RL a) (RL a)
crlthrus = crlpasss

mkid :: a -> Identity a
mkid = Identity

unid :: Identity a -> a
unid (Identity x) = x

class (Functor w) => Comonad w where
    coreturn :: w a -> a
    cojoin :: w a -> w (w a)
    cojoin = cobind id
    cobind :: (w a -> b) -> w a -> w b
    cobind f = fmap f . cojoin
    infixl 1 `cobind`

instance Comonad Identity where { coreturn = unid; cojoin = mkid; }

-- | Isomorphism.
data Iso a b = MkIso { _isoright :: a -> b, _isoleft :: b -> a }
isorev :: Iso a b -> Iso b a
isorev (MkIso f g) = MkIso g f
bimapCon :: Iso r s -> ((a -> r) -> r) -> ((a -> s) -> s)
bimapCon i f cs =
    right_ (f (left_ . cs))
    where
        left_ = _isoleft i
        right_ = _isoright i
liftCont :: (Monad m, Comonad m) => ((a -> r) -> r) -> ((a -> m r) -> m r)
liftCont = bimapCon iso
    where
        iso = MkIso return coreturn
lowerCont :: (Monad m, Comonad m) => ((a -> m r) -> m r) -> ((a -> r) -> r)
lowerCont = bimapCon iso
    where
        iso = MkIso coreturn return

newtype Build r a
    = MkBuild { _runBuild :: Ms.StateT Bst (Mc.ContT r Identity) a }
data Bst
    = MkBst
    {
        _bstrate :: !(Rate Int)
        , _bstTickPerBeat :: !(Rate Int)
        , _bstsamno :: !Int
        , _bsttick :: !Int
        , _bstticksam :: L (SampleNumber Int)
    }
    deriving (Show)
instance Functor (Build r) where fmap f = MkBuild . fmap f . _runBuild
instance Applicative (Build r) where { pure = MkBuild . pure; (<*>) ff fx = MkBuild (_runBuild ff <*> _runBuild fx); }
instance Monad (Build r) where { return = MkBuild . return; (>>=) m k = MkBuild (_runBuild m >>= _runBuild . k); }
instance (Num a) => Num (Build r a) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
instance (Fractional a) => Fractional (Build r a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational
bmodify_ :: (Bst -> Bst) -> Build r ()
bmodify_ f = MkBuild $ Ms.modify f
bstupdatetick_ :: Bst -> Bst
bstupdatetick_ s =
    if nextTickSampleNumber > physamno
        then s
        else bstupdatetick_ s'
    where
        tick = _bsttick s
        physamno = _bstsamno s
        MkL _ (MkL nextTickSampleNumber ticksam') = _bstticksam s
        s' = s
            {
                _bsttick = succ tick
                , _bstticksam = ticksam'
            }
badvsam_ :: Int -> Build r ()
badvsam_ n =
    if n >= 0
        then bmodify_ $ \ s -> bstupdatetick_ s { _bstsamno = _bstsamno s + n }
        else error "badvsam_: trying to go back in time"
basks_ :: (Bst -> a) -> Build r a
basks_ f = MkBuild (Ms.gets f)
runBuild :: Build r a -> Bst -> ((a, Bst) -> Identity r) -> Identity r
runBuild x s c = Mc.runContT (Ms.runStateT (_runBuild x) s) c
buildI_ :: Bst -> Build a a -> a
buildI_ s x = unid (Mc.runContT (Ms.evalStateT (_runBuild x) s) return)
buildl :: SamplePerSecond (Rate Int) -> TickPerBeat (Rate Int) -> L (SampleNumber Int) -> Build (L a) (L a) -> L a
buildl r tpb logtophy x = buildI_ (MkBst r tpb 0 0 logtophy) x
buildrl :: SamplePerSecond (Rate Int) -> TickPerBeat (Rate Int) -> L (SampleNumber Int) -> Build (L a) (L a) -> RL a
buildrl r tpb ltp x = rated r $ buildl r tpb ltp x

-- | Get the current sample number.
bsamno :: Build r Int
bsamno = basks_ _bstsamno
-- | Get the sample rate.
brate :: Build r (Rate Int)
brate = basks_ _bstrate

-- | Lift a continuation.
bcont :: ((a -> r) -> r) -> Build r a
bcont x =
    MkBuild . Ms.StateT $ \ s ->
        Mc.ContT (\ c -> return (x (\ a -> coreturn (c (a, s)))))

{- |
@blwrite n x@ writes the first @n@ samples of @x@ to the output stream.
This returns @'ldrop' n x@.
-}
blwrite :: Build (L a) Int -> L a -> Build (L a) (L a)
blwrite bn x = bn >>= \ n -> badvsam_ n >> bcont (lcid n x)

-- | This is like 'blwrite' but discards the stream tail.
blwrite_ :: Build (L a) Int -> L a -> Build (L a) ()
blwrite_ bn x = blwrite bn x >> return ()

{- |
@blwritemap n f x@ writes the first @n@ samples of @'lmap' f x@ to the output stream.
This returns @'ldrop' n x@.
-}
blwritemap :: Build (L b) Int -> (a -> b) -> L a -> Build (L b) (L a)
blwritemap bn f x = bn >>= \ n -> badvsam_ n >> bcont (lcmap n f x)

{- |
@blwritezip n f x y@ writes the first @n@ samples of @'lzip' f x y@ to the output stream.
This returns @('ldrop' n x, 'ldrop' n y)@.
-}
blwritezip :: Build (L c) Int -> (a -> b -> c) -> L a -> L b -> Build (L c) (L a, L b)
blwritezip bn f x y = bn >>= \ n -> badvsam_ n >> bcont (lczip n f x y . curry)

blint :: (Fractional a) => L a -> Build r (L a)
blint x = fmap (\ r -> lint (ratedt r) x) brate

-- | Literal constants do not need this due to @('Num' a) => 'Num' ('Build' r a)@.
bsam :: Int -> Build r Int
bsam = pure

-- | Convert number of seconds to number of samples.
bsec :: T -> Build r Int
bsec t = fmap (round . (* t) . fromIntegral . _unRate) brate

-- | Convert number of ticks into number of samples.
btick :: Int -> Build r Int
btick t =
    basks_ $ \ s ->
        let
            lookupStream = _bstticksam s
            currentSample = _bstsamno s
            targetSample = head (ldrop t lookupStream)
        in
            targetSample - currentSample

bbeat :: Rational -> Build r Int
bbeat n = do
    tpb <- basks_ (_unRate . _bstTickPerBeat)
    btick (round (n * fromIntegral tpb))

{- |
@bto n@ computes the number of samples that we must write
to make the current sample number be @n@.

See 'bsamno' for the current sample number.
-}
bto :: Int -> Build r Int
bto n = fmap (\ s -> n - s) bsamno

btos :: T -> Build r Int
btos = bsec >=> bto
