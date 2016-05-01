{- |
Allow only one thread among many to execute a critical section.
-}
module Mutex
(
    Mutex
    , new
    , acquire
    , release
    , with
)
where

import qualified Control.Monad.IO.Class as Ic

import qualified Control.Concurrent as Co
import qualified Control.Exception as Ex

{- |
Mutual-exclusion lock.
-}
newtype Mutex
    = MkMutex Co.QSem

{- |
Make a new unlocked mutex.
-}
new :: (Ic.MonadIO m) => m Mutex
new = Ic.liftIO $ MkMutex <$> Co.newQSem 1

{- |
Lock the mutex.
-}
acquire :: (Ic.MonadIO m) => Mutex -> m ()
acquire (MkMutex sem) = Ic.liftIO $ Co.waitQSem sem

{- |
Unlock the mutex.
-}
release :: (Ic.MonadIO m) => Mutex -> m ()
release (MkMutex sem) = Ic.liftIO $ Co.signalQSem sem

{- |
This is 'acquire' before entering and 'release' after leaving.
-}
with :: (Ic.MonadIO m) => Mutex -> IO a -> m a
with mut = Ic.liftIO . Ex.bracket_ (acquire mut) (release mut)
