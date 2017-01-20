{- |
Deprecated: Use "Dynasty.Random.Uniform" instead.
-}
module Dynasty.Random
where

import qualified Control.Monad as M

import qualified Control.Monad.Trans.State as S

import qualified System.Random as R

class (Monad m) => MonadRandom m where

    uniformInt :: Int -> m Int

    uniformUnit :: m Probability

instance MonadRandom IO where

    uniformInt n = R.randomRIO (0, n - 1)

    uniformUnit = R.randomIO

instance (MonadRandom m) => MonadRandom (S.StateT s m) where

    uniformInt = lift . uniformInt

    uniformUnit = lift uniformUnit

lift :: (Functor m) => m a -> S.StateT s m a
lift m = S.StateT $ \ s -> fmap (\ x -> (x, s)) m

type Probability = Double

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
