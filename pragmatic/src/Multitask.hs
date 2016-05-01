module Multitask where

import qualified Control.Concurrent.QSem as Sem
import qualified Control.Exception as Ex

import qualified Control.Concurrent.Async as Ca

concurrently :: IO a -> IO b -> IO (a, b)
concurrently = Ca.concurrently

{- |
This should handle Ctrl-C in GHCI correctly.
The package parallel-io (Control.Concurrent.ParallelIO.Local) doesn't.
-}
concurrent
    :: Int -- ^ number of threads to spawn; if this is non-positive, 'concurrent' will use one thread
    -> [IO a] -- ^ computations
    -> IO [a] -- ^ results, in the same order as the computations

concurrent limit jobs | limit < 1 = sequence jobs
concurrent limit jobs = do
    sem <- Sem.newQSem limit
    Ca.mapConcurrently (with sem) jobs
    where
        with sem = Ex.bracket_ (Sem.waitQSem sem) (Sem.signalQSem sem)
