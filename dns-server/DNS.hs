{-# LANGUAGE OverloadedStrings #-}
module DNS
(
    main
    , interactiveMain
)
where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Text.Read as R
import qualified System.Environment as EN
import qualified System.IO.Error as IE

import qualified Network.HTTP.Client as H

import qualified Network.Socket as S

import DNS.Server
import DNS.Web
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
    server $ case args of
        strPort : _ -> maybe defPort intPort $ R.readMaybe strPort
        _ -> defPort
    where
        defPort = 1053
        intPort :: Int -> S.PortNumber
        intPort = fromIntegral

server :: S.PortNumber -> IO ()
server port = do
    socket <- DS.newUdpSocket
    udpBindAddress <- S.SockAddrInet port <$> S.inet_addr "127.0.0.1"
    S.bind socket udpBindAddress
    putStrLn $ "DNS server bound to UDP " ++ show udpBindAddress
    H.withManager managerSettings $ \ manager -> do
        forever $ flip IE.catchIOError print $ do
            packet <- DS.recvFrom socket
            let request = DS._payload packet
            -- print request -- debug
            response <- lookupA manager request
            DS.sendTo socket packet { DS._payload = answerMinTtl 86400 $ response }
    where
        -- FIXME wireshark shows that http-client closes the connection (ignoring Connection: keep-alive)
        managerSettings = H.defaultManagerSettings
            {
                H.managerConnCount = 1
                , H.managerResponseTimeout = Just 10000000 -- microseconds
                , H.managerIdleConnectionCount = 1
            }
