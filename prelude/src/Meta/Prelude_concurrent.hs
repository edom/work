{- |
This assumes that the compiler is GHC.

See also "Control.Concurrent".

We can make a mutex from a 'QSem' with initial count 1.

* Don't use GHC 7.6:

    * https://ghc.haskell.org/trac/ghc/ticket/3160

    * https://github.com/ChrisKuklewicz/SafeSemaphore

* Related libraries:

    * https://hackage.haskell.org/package/broadcast-chan
-}
module Meta.Prelude_concurrent (
    -- ** Forking
    C.forkIO
    , C.forkOS
    , C.killThread
    , C.threadDelay
    -- ** Semaphore
    , module Control.Concurrent.QSem
    , module Control.Concurrent.MVar
    , withQSem
    -- ** Lock
    , Lock
    , new_lock
    , with_lock
) where

import Control.Concurrent.MVar
import Control.Concurrent.QSem

import qualified Control.Concurrent as C
import qualified Control.Exception as E

{- |
@withQSem semaphore action@ runs the action while owning one count of the semaphore.

This uses 'E.bracket' so the lock should be released even if the action throws an exception, but we aren't sure.

We don't know whether this is also a memory barrier.

We don't know how to prove the correctness of this thing.
We advise you to mitigate the risk by periodically health-checking your program and restarting your program.
-}
withQSem :: QSem -> IO a -> IO a
withQSem sem action = E.bracket (waitQSem sem) (\ _ -> signalQSem sem) (\ _ -> action)

{- |
The lock is non-reentrant.
This program deadlocks:

@
'with_lock' lock ('with_lock' lock action)
@

See 'withQSem' for non-guarantees.

The implementation uses 'QSem' with count 1, but we may change the implementation as long as it doesn't change the semantics described here.
-}
newtype Lock = MkLock QSem

-- | See 'Lock'.
new_lock :: IO Lock
new_lock = MkLock <$> newQSem 1

{- |
If multiple threads are running @with_lock lock action@, then they run @action@ without overlapping in time.
-}
with_lock :: Lock -> IO a -> IO a
with_lock (MkLock sem) = withQSem sem
