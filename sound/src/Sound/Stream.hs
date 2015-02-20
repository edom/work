{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{- |
This module is similar to "Data.List".
-}
module Sound.Stream
(
    -- * Construction
    Stream
)
where

import Control.Applicative

import Sound.Class
import Sound.Pair

-- | An infinite sequence of values of one type.
data Stream a =
    forall s. MkStream
    {
        _sstep :: !(s -> P a s)
        , _sstate :: !s
    }

instance Point Stream where
    point = srepeat
    {-# INLINE point #-}

instance Functor Stream where
    fmap f (MkStream y x) = MkStream (pmap0 f . y) x
    {-# INLINE fmap #-}

instance Applicative Stream where
    pure = point
    (<*>) = zip2 ($)

srepeat :: a -> Stream a
srepeat x = MkStream (\ u -> MkP u u) x
{-# INLINE srepeat #-}

instance Zip Stream where
    zip2 f (MkStream stepx statx) (MkStream stepy staty) = MkStream step (MkP statx staty)
        where
            step (MkP sx sy) = 
                MkP (f a b) (MkP sx' sy')
                where
                    MkP a sx' = stepx sx
                    MkP b sy' = stepy sy
    {-# INLINE zip2 #-}
