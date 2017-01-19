module Dynasty.Random
(
    -- * Monad

    MonadRandom(..)

    -- * Bernoulli trial

    , Probability
    , bernoulli
    , probM
)
where

import qualified Control.Monad as M

import qualified Control.Monad.Trans.State as S

import qualified System.Random as R

class (Monad m) => MonadRandom m where

    {- |
    The expression @uniform n@ represents the uniform distribution
    of integers between 0 inclusive and n exclusive.
    -}

    uniformInt :: Int -> m Int

    {- |
    Generate a double-precision floating-point integer
    between 0 inclusive and 1 exclusive.
    -}

    uniformUnit :: m Probability

instance MonadRandom IO where

    uniformInt n = R.randomRIO (0, n - 1)

    uniformUnit = R.randomIO

instance (MonadRandom m) => MonadRandom (S.StateT s m) where

    uniformInt = lift . uniformInt

    uniformUnit = lift uniformUnit

lift :: (Functor m) => m a -> S.StateT s m a
lift m = S.StateT $ \ s -> fmap (\ x -> (x, s)) m

{- |
A double-precision floating-point number between 0 inclusive and 1 inclusive.
-}
type Probability = Double

{- |
The expression @bernoulli p x y@ represents
a single trial with two outcomes @x@ and @y@
where the probability of @x@ is @1 - p@
and the probability of @y@ is @p@.

It is like a possibly unfair coin flip.
-}
bernoulli :: (MonadRandom m) => Probability -> a -> a -> m a
bernoulli p x y = do
    r <- uniformUnit
    return $ if r < p then y else x

{- |
The expression @probM p x@
represents a chance of @p@ of executing @x@.
-}
probM :: (MonadRandom m) => Probability -> m () -> m ()
probM p x = M.join $ bernoulli p nothing x
    where
        nothing = return ()
