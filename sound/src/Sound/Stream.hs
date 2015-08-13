{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{- |
This module is similar to "Data.List".

When a good producer meets a good consumer,
GHC can inline them and eliminate the Stream data constructor,
unboxing the state.

Performance notes:

* If you make a stream with a big state, you may need @-fmax-worker-args=32@ or higher.

* If you bind a stream to a name, sometimes you may need to @\{\-\# INLINE \#\-\}@ it.
-}
module Sound.Stream
(
    -- * Construction
    Stream
    , mmap
    , unfold
    , munfold
    , repeat
    , iterate
    , zip2
    , sine
    , intMod1
    , fm
    , cons
    -- * Consumption
    , takeMapM_
    , consumeS
)
where

import Control.Applicative
import Prelude (($), (.), Functor(..)
    , Num(..), Fractional(..), Floating(..)
    , IO, Bool(..), Double, Monad(..), Ord(..), Int)
import qualified Prelude as P

import qualified Control.DeepSeq as D
import qualified Control.Monad as M
import qualified Foreign as F

import qualified Sound.Procedural as T

data Stream m a =
    forall s. (D.NFData s) => MkStream
    {
        _ts :: !s -- ^ state
        , _te :: !(s -> m s) -- ^ state endofunction
        , _to :: !(s -> m a) -- ^ output mapper
    }

instance (Functor m) => Functor (Stream m) where
    fmap f (MkStream s e o) = MkStream s e (fmap f . o)
    {-# INLINE fmap #-}

instance (Applicative m, Num a) => Num (Stream m a) where
    (+) = zip2 (+)
    (*) = zip2 (*)
    (-) = zip2 (-)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = repeat . fromInteger
    {-# INLINE (+) #-}
    {-# INLINE (*) #-}
    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    {-# INLINE abs #-}
    {-# INLINE signum #-}
    {-# INLINE fromInteger #-}

instance (Fractional a, Applicative m) => Fractional (Stream m a) where
    {-# SPECIALIZE instance Fractional (Stream IO Double) #-}
    (/) = zip2 (/)
    recip = fmap recip
    fromRational x = case fromRational x of !y -> MkStream () pure (\ _ -> pure y)
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    {-# INLINE fromRational #-}

unfold :: (Applicative m, D.NFData s) => s -> (s -> s) -> (s -> a) -> Stream m a
unfold seed endo out = MkStream seed (pure . endo) (pure . out)

munfold :: (D.NFData s) => s -> (s -> m s) -> (s -> m a) -> Stream m a
munfold = MkStream

repeat :: (Applicative m) => a -> Stream m a
repeat x = munfold () pure out
    where
        out _ = pure x

iterate :: (D.NFData a, Applicative m) => a -> (a -> a) -> Stream m a
iterate x f = munfold x (pure . f) pure

{-# INLINE zip2 #-}
zip2 :: (Applicative m) => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zip2 f (MkStream s0 e0 o0) (MkStream s1 e1 o1) = MkStream s2 e2 o2
    where
        s2 = (s0, s1)
        e2 (s, t) = (,) <$> e0 s <*> e1 t
        o2 (s, t) = f <$> o0 s <*> o1 t

{-# INLINE sine #-}
sine :: (Floating a, Applicative m) => a -> a -> Stream m a
sine !rate !freq =
    fun <$> iterate z (1 +)
    where
        z :: Int
        !z = 0
        !w = 2 * P.pi * freq / rate
        fun !i = P.sin $ w * P.fromIntegral i

{-# INLINE intMod1 #-}
intMod1 :: (D.NFData a, Ord a, Fractional a, Applicative m) => a -> Stream m a -> Stream m a
intMod1 !rate (MkStream s0 e0 o0) = MkStream s1 e1 o1
    where
        !dt = P.recip rate
        s1 = (0, s0)
        e1 (a, s) = (,) <$> fmap (fun a) (o0 s) <*> e0 s
        o1 (a, _) = pure a
        fun a x =
            if y >= 1
                then y - 1
                else y
            where
                y = a + x * dt

{-# INLINE fm #-}
fm :: (F.Storable a, D.NFData a, P.RealFrac a) => a -> T.Table a -> Stream IO a -> Stream IO a
fm rate table freq = mmap (T.unsafeTlookup table) $ intMod1 rate freq

{- |
This is one-cell delay line.

This cannot be used recursively.
For example, this compiles into an infinite loop,
contrary to what you might expect:

@
y = 0.75 * x + 0.25 * cons 0 y
@
-}
cons :: (Applicative m, D.NFData a) => a -> Stream m a -> Stream m a
cons first (MkStream s0 e0 o0) = MkStream s1 e1 o1
    where
        s1 = (first, s0)
        e1 (_, s) = (,) <$> o0 s <*> e0 s
        o1 (x, _) = pure x

mmap :: (Monad m) => (a -> m b) -> Stream m a -> Stream m b
mmap k (MkStream s e o) = MkStream s e (o M.>=> k)

{- |
@
takeMapM_ n k s ~ 'P.take' n ('P.mapM_' k s)
@
-}
takeMapM_ :: (Monad m) => Int -> (a -> m b) -> Stream m a -> m ()
takeMapM_ !n !k (MkStream s0 e o) = loop 0 s0
    where
        loop !i !s =
            D.deepseq s $ do
                M.when (i < n) $ do
                    !t <- e s
                    !x <- o s
                    !_ <- k x
                    loop (i + 1) t

{-# INLINE consumeS #-}
consumeS :: (F.Storable a) => Stream IO a -> (a -> IO Bool) -> IO ()
consumeS (MkStream s0 e o) act = loop s0
    where
        loop !s = do
            !a <- o s
            !cont <- act a
            s' <- e s
            M.when cont $ loop s'
