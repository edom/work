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

import qualified Data.ByteString.Lazy as BSL

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB

import qualified Network.DNS as D

import qualified Network.HTTP.Client as H

import DNS.Web

-- | For testing with GHCI. Pressing Enter should terminate the server.
interactiveMain :: IO ()
interactiveMain = S.withSocketsDo $ do
    child <- forkIO actualMain
    _ <- getLine
    killThread child

main :: IO ()
main = S.withSocketsDo actualMain

newUdpSocket :: IO S.Socket
newUdpSocket = do
    socket <- S.socket S.AF_INET S.Datagram S.defaultProtocol
    S.setSocketOption socket S.ReuseAddr 1
    return socket

data Packet
    = MkPacket
    {
        _qPeer :: S.SockAddr
        , _qPayload :: D.DNSFormat
    }
    deriving (Show)

recvPacketFrom :: S.Socket -> IO Packet
recvPacketFrom socket = do
    (bsRequest, peerAddress) <- SB.recvFrom socket 512
    either (IE.ioError . IE.userError) (return . MkPacket peerAddress)
        $ D.decode $ BSL.fromStrict bsRequest

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
    socket <- newUdpSocket
    udpBindAddress <- S.SockAddrInet port <$> S.inet_addr "127.0.0.1"
    S.bind socket udpBindAddress
    putStrLn $ "DNS server bound to UDP " ++ show udpBindAddress
    H.withManager managerSettings $ \ manager -> do
        forever $ flip IE.catchIOError print $ do
            packet <- recvPacketFrom socket
            let request = _qPayload packet
                peerAddress = _qPeer packet
            -- print request -- debug
            response <- lookupA manager request
            SB.sendAllTo socket (BSL.toStrict $ D.encode response) peerAddress
    where
        -- FIXME wireshark shows that http-client closes the connection (ignoring Connection: keep-alive)
        managerSettings = H.defaultManagerSettings
            {
                H.managerConnCount = 1
                , H.managerResponseTimeout = Just 10000000 -- microseconds
                , H.managerIdleConnectionCount = 1
            }
