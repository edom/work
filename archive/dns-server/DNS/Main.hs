{-# LANGUAGE OverloadedStrings #-}
module DNS.Main
(
    main
    , interactiveMain
)
where

import Control.Concurrent
import Control.Monad
import qualified Control.Concurrent as Con
import qualified System.Environment as EN
import qualified System.IO.Error as IE

import qualified Network.HTTP.Client as H

import qualified Network.Socket as S

import qualified DNS.Config as C
import qualified DNS.Log as L
import qualified DNS.Resolve as R
import qualified DNS.Resolve.Web as W
import qualified DNS.Server as SV
import qualified DNS.Socket as DS

-- | For testing with GHCI. Pressing Enter should terminate the server.
interactiveMain :: IO ()
interactiveMain = S.withSocketsDo $ do
    child <- forkIO actualMain
    _ <- getLine
    killThread child

main :: IO ()
main = S.withSocketsDo actualMain

actualMain :: IO ()
actualMain = do
    args <- EN.getArgs
    either (IE.ioError . IE.userError) server $ C.fromArgs args

server :: C.Config -> IO ()
server config = do
    logchan <- Con.newChan
    socket <- DS.newBoundUdpSocket "127.0.0.1" port
    udpBindAddress <- S.getSocketName socket
    putStrLn $ "DNS server bound to UDP " ++ show udpBindAddress
    _ <- forkIO $ forever $ Con.readChan logchan >>= putStrLn
        -- XXX if this thread dies (such as by writeChan logchan (error "boo")), logchan queue grows unbounded
    H.withManager managerSettings $ \ manager -> do
        let resolve = R.mapResolver (maybe id SV.answerMinTtl answerMinTtl) $ W.query manager
        forever $
            flip IE.catchIOError print $ R.onceSocket socket (L.chan logchan) resolve
            >> return ()
    where
        port = C.port config
        answerMinTtl = C.answerMinTtl config
        -- FIXME wireshark shows that http-client closes the connection (ignoring Connection: keep-alive)
        managerSettings = H.defaultManagerSettings
            {
                H.managerConnCount = 1
                , H.managerResponseTimeout = Just 10000000 -- microseconds
                , H.managerIdleConnectionCount = 1
            }
