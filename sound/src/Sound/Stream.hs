{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
This module is similar to "Data.List".
-}
module Sound.Stream
(
    -- * Construction
    Stream
    , StreamM
)
where

import Control.Applicative

import Sound.Class
import Sound.Pair

-- | An infinite sequence of values of one type.
data Stream a =
    forall s. MkStream
    {
        _se :: !(s -> s) -- state endofunction
        , _so :: !(s -> a) -- output mapper
        , _ss :: !s -- state
    }

instance Point Stream where
    point x = MkStream { _se = id, _so = id, _ss = x }
    {-# INLINE point #-}

instance Functor Stream where
    fmap f (MkStream e o s) = MkStream e (f . o) s
    {-# INLINE fmap #-}

instance Applicative Stream where
    pure = point
    (<*>) = zip2 ($)

instance Head Stream where head (MkStream _ o s) = o s
instance Tail Stream where tail (MkStream e o s) = MkStream e o (e s)
instance Decons Stream
instance DeconsM m Stream where deconsM = decons
instance Consume Stream
instance Fill Stream

instance Zip2 Stream where
    zip2 f (MkStream { _se = ex, _so = ox, _ss = sx }) (MkStream { _se = ey, _so = oy, _ss = sy }) =
        MkStream { _se = e, _so = o, _ss = s }
        where
            s = MkP sx sy
            e (MkP u v) = MkP (ex u) (ey v)
            o (MkP u v) = f (ox u) (oy v)
    {-# INLINE zip2 #-}

data StreamM m a =
    forall s. MkStreamM
    {
        _te :: !(s -> m s) -- state endofunction
        , _to :: !(s -> m a) -- output mapper
        , _ts :: !s -- state
    }

instance (Point m) => Point (StreamM m) where
    point x = MkStreamM { _te = point, _to = point, _ts = x }
    {-# INLINE point #-}

instance (Functor m) => Functor (StreamM m) where
    fmap f (MkStreamM { _te = e, _to = o, _ts = s }) = MkStreamM { _te = e, _to = fmap f . o, _ts = s }
    {-# INLINE fmap #-}
