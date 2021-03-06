{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Sound.StreamOld
(
    Stream
)
where

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
instance Unfold Stream where unfold o e s = MkStream e o s
instance FromList Stream where fromList = fromList_unfold
instance Drop Stream where
    drop n (MkStream e o s) = MkStream e o (c n e s)
        where
            c k _ _ | k < 0 = error "Drop Stream drop: negative"
            c 0 _ x = x
            c k f x = c (k - 1) f (f x)

instance Scan Stream where
    scanl f a (MkStream e o s) =
        MkStream (uncurry (\ a_ s_ -> MkP (f a_ (o s_)) (e s_))) proj0 (MkP a s)
    {-# INLINE scanl #-}

instance Zip2 Stream where
    zip2 f (MkStream { _se = ex, _so = ox, _ss = sx }) (MkStream { _se = ey, _so = oy, _ss = sy }) =
        MkStream { _se = e, _so = o, _ss = s }
        where
            s = MkP sx sy
            e (MkP u v) = MkP (ex u) (ey v)
            o (MkP u v) = f (ox u) (oy v)
    {-# INLINE zip2 #-}
