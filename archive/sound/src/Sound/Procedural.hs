{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Sound.Procedural
(
    -- * Construction
    consDouble
    -- * Integration
    , int
    -- * Array
    -- $array
    , newBoolArray
    , newIntArray
    , newDoubleArray
    , withDoubleArray
    , example
    -- * Types
    , G
)
where

import Control.Applicative
import Foreign

import qualified Data.Array.IO.Safe as AI
import qualified Data.Array.MArray.Safe as AM

import Sound.Class
import Sound.Buffer

newtype G m a = MkG (m a)

instance (Point m) => Point (G m) where point = mkG . point
instance (Functor m) => Functor (G m) where fmap f g = mkG $ fmap f $ _step g
instance Consume (G IO)
instance Fill (G IO)

instance (Monad m) => DeconsM m (G m) where
    deconsM x f = do
        h <- _step x
        f h x

instance (Applicative m) => Applicative (G m) where
    pure = mkG . pure
    mf <*> mx = mkG $ _step mf <*> _step mx

instance (Monad m) => Monad (G m) where
    return = mkG . return
    m >>= k = mkG $ _step m >>= _step . k

instance (Applicative m) => Zip2 (G m) where
    zip2 f x y = mkG $ f <$> _step x <*> _step y
    {-# INLINE zip2 #-}

mkG :: m a -> G m a
mkG = MkG

_step :: G m a -> m a
_step (MkG x) = x

{- $array
See also:

* "Data.Array.IO.Safe"

* "Data.Array.MArray.Safe"
-}

newBoolArray :: Int -> IO (AI.IOUArray Int Bool)
newBoolArray n = AM.newArray (0, n - 1) False

newDoubleArray :: Int -> IO (AI.IOUArray Int Double)
newDoubleArray n = AM.newArray (0, n - 1) 0

newIntArray :: Int -> IO (AI.IOUArray Int Double)
newIntArray n = AM.newArray (0, n - 1) 0

withDoubleArray :: Int -> (AI.IOUArray Int Double -> IO a) -> IO a
withDoubleArray n k = newDoubleArray n >>= k

int :: Double -> G IO Double -> IO (G IO Double)
int !dt !g = do
    !s <- newDoubleArray 1
    return $ mkG $ do
        !x <- stepg
        !y <- AM.readArray s 0
        AM.writeArray s 0 $! y + x * dt
        return y
    where
        stepg = _step g
{-# INLINE int #-}

consDouble :: Double -> G IO Double -> IO (G IO Double)
consDouble !h !g = do
    !s <- newIntArray 1
    return . mkG $ do
        !b <- AM.readArray s 0
        case b of
            0 -> do
                AM.writeArray s 0 1
                return h
            _ -> _step g
{-# INLINE consDouble #-}

example :: IO ()
example = do
    allocaBuffer 1024 $ \ a -> do
        !g <- int dt (point 1) >>= int dt >>= consDouble 123 >>= int dt
        -- let !g = point 123 :: G IO Double
        -- !g <- int dt (point 1) >>= consDouble 123
        fill a g >> return ()
        !x <- peekElemOff (bufPtr a) 0
        print x
    where
        dt :: Double
        dt = recip 44100
