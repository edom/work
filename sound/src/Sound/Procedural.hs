{-# LANGUAGE Safe #-}
{-# LANGUAGE BangPatterns #-}

module Sound.Procedural
(
    -- * Construction
    gdef
    , gconst
    , cons
    -- * Mapping
    , map
    -- * Combination
    , zip2
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
    , mkG
)
where

import Control.Applicative
import Prelude hiding (map)

import qualified Data.Array.IO.Safe as AI
import qualified Data.Array.MArray.Safe as AM

type G m a = m a

mkG :: m a -> G m a
mkG = id

_step :: G m a -> m a
_step = id

gdef :: (Applicative m) => a -> G m a
gdef = mkG . pure

gconst :: (Applicative m) => a -> G m a
gconst = gdef

run :: (AM.MArray a e m, Monad m) => a Int e -> Int -> G m e -> m ()
run a !n g = loop 0
    where
        step = _step g
        loop !i
            | i >= n = return ()
            | otherwise = do
                x <- step
                AM.writeArray a i x
                loop (i + 1)

fill :: (AM.MArray a e m, Functor m, Monad m) => a Int e -> G m e -> m ()
fill a !g = do
    !n <- AM.rangeSize <$> AM.getBounds a
    run a n g

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

map :: (Functor m) => (a -> b) -> G m a -> G m b
map f g = mkG $ fmap f $ _step g

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

cons :: Double -> G IO Double -> IO (G IO Double)
cons !h !g = do
    !s <- newIntArray 1
    return $ do
        !b <- AM.readArray s 0
        case b of
            0 -> do
                AM.writeArray s 0 1
                return h
            _ -> g
{-# INLINE cons #-}

zip2 :: (Applicative m) => (a -> b -> c) -> G m a -> G m b -> G m c
zip2 f x y = mkG $ f <$> _step x <*> _step y

example :: IO ()
example = do
    !a <- newDoubleArray 1024
    !g <- int dt (gdef 1) >>= int dt >>= cons 123 >>= int dt
    -- !g <- int dt (gdef 1) >>= cons 123
    fill a g
    !x <- AM.readArray a 0
    print x
    where
        dt :: Double
        dt = recip 44100
