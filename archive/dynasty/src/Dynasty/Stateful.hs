{- |
Various ways of implementing statefulness using monads.

This uses explicit type class dictionary.
-}
module Dynasty.Stateful
(
    -- * Class

    Stateful(..)

    , gets
    , modify
    , modifyM

    -- * Instances

    , stateT
    , ioRef
)
where

import qualified Control.Monad.IO.Class as I
import qualified Data.IORef as J

import qualified Control.Monad.Trans.State as M

data Stateful s m
    = MkStateful
    {
        get :: m s
        , put :: s -> m ()
    }

gets :: (Functor m) => Stateful s m -> (s -> a) -> m a
gets c f = f <$> get c

modify :: (Monad m) => Stateful s m -> (s -> s) -> m ()
modify imp f = get imp >>= put imp . f

modifyM :: (Monad m) => Stateful s m -> (s -> m s) -> m ()
modifyM imp k = get imp >>= k >>= put imp

stateT :: (Monad m) => Stateful s (M.StateT s m)
stateT = MkStateful
    M.get
    M.put

ioRef :: (I.MonadIO m) => J.IORef s -> Stateful s m
ioRef var = MkStateful
    (I.liftIO $ J.readIORef var)
    (I.liftIO . J.writeIORef var)
