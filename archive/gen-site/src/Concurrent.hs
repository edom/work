module Concurrent
where

{-

import qualified Control.Concurrent.QSem as Sem
import qualified Control.Exception as E

import qualified Control.Concurrent.ParallelIO.Local as Par

import qualified GHC.Conc as Gco

{- |
The number of threads that can be used by 'parForM' and 'parForM_'.

If you are using GHC's threaded runtime, you can change this with RTS option @-N@.

If you are using stack ghci and you want to use all cores,
you can use @stack ghci --ghc-options='+RTS -N -RTS'@.
-}
numThreads :: Int
numThreads = Gco.numCapabilities

parallelE :: [IO a] -> IO [Either E.SomeException a]
parallelE = Par.withPool numThreads . flip Par.parallelE

parallelE_ :: [IO a] -> IO [Maybe E.SomeException]
parallelE_ = Par.withPool numThreads . flip Par.parallelE_

parallel :: [IO a] -> IO [a]
parallel = Par.withPool numThreads . flip Par.parallel

parallel_ :: [IO a] -> IO ()
parallel_ = Par.withPool numThreads . flip Par.parallel_

{- |
Like @forM@ from "Control.Monad" but parallel.

This uses up to 'numThreads' threads.
-}
parForM :: [a] -> (a -> IO b) -> IO [b]
parForM list act =
    Par.withPool numThreads $ flip Par.parallel $ map act list

parMapM :: (a -> IO b) -> [a] -> IO [b]
parMapM act list =
    Par.withPool numThreads $ flip Par.parallel $ map act list

{- |
Like @forM_@ from "Control.Monad" but parallel.

This uses up to 'numThreads' threads.
-}
parForM_ :: [a] -> (a -> IO b) -> IO ()
parForM_ list act =
    Par.withPool numThreads $ flip Par.parallel_ $ map act list

-- * Semaphored putStrLn

mkPutStrLn :: IO (String -> IO ())
mkPutStrLn = do
    sem <- Sem.newQSem 1
    return $ withQSem sem . putStrLn

withQSem :: Sem.QSem -> IO a -> IO a
withQSem sem = E.bracket_ (Sem.waitQSem sem) (Sem.signalQSem sem)

-}
