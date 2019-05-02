module DNS.Log
(
    Logger
    , discard
    , stdout
    , chan
)
where

import qualified Control.Concurrent as C

type Logger = String -> IO ()

discard :: Logger
discard = const $ return ()

stdout :: Logger
stdout = putStrLn

chan :: C.Chan String -> Logger
chan = C.writeChan
