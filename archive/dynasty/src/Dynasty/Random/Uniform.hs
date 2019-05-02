module Dynasty.Random.Uniform
(
    Uniform

    , sample

    , Probability

    , unit
    , int

    , bernoulli
)
where

import qualified Control.Monad.IO.Class as I

import qualified System.Random as R

newtype Uniform m a = In { out :: m a }

{- |
Take a sample from the distribution.
-}
sample :: Uniform m a -> m a
sample = out

{- |
A double-precision floating-point number between 0 inclusive and 1 inclusive.
-}
type Probability = Double

{- |
Uniform distribution of double-precision floating-point integers
between 0 inclusive and 1 exclusive.
-}
unit :: (I.MonadIO m) => Uniform m Probability
unit = In $ I.liftIO $ R.randomIO

{- |
The expression @int n@ represents the uniform distribution
of integers between 0 inclusive and n exclusive.
-}
int :: (I.MonadIO m) => Int -> Uniform m Int
int n = In $ I.liftIO $ R.randomRIO (0, n - 1)

{- |
The expression @bernoulli p x y@ represents
a single trial with two outcomes @x@ and @y@
where the probability of @x@ is @1 - p@
and the probability of @y@ is @p@.

It is like a possibly unfair coin flip.
-}
bernoulli :: (I.MonadIO m) => Probability -> a -> a -> m a
bernoulli p x y = do
    r <- sample unit
    return $ if r < p then y else x
