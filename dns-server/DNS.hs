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
import Text.Printf
import qualified System.Environment as EN
import qualified System.IO.Error as IE

import qualified Data.ByteString.Char8 as BSC

import qualified Network.HTTP.Client as H

import qualified Network.DNS as D

import qualified Network.Socket as S

import qualified Data.Time as T

import DNS.Error
import qualified DNS.Config as C
import qualified DNS.Server as SV
import qualified DNS.Socket as DS
import qualified DNS.Web as W

timed :: IO a -> IO (T.NominalDiffTime, a)
timed action = do
    t <- T.getCurrentTime
    x <- action
    u <- T.getCurrentTime
    let d = T.diffUTCTime u t -- picoseconds
    return (d, x)

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
    socket <- DS.newUdpSocket
    udpBindAddress <- S.SockAddrInet port <$> S.inet_addr "127.0.0.1"
    S.bind socket udpBindAddress
    putStrLn $ "DNS server bound to UDP " ++ show udpBindAddress
    H.withManager managerSettings $ \ manager -> do
        forever $ flip IE.catchIOError print $ do
            packet <- DS.recvFrom socket
            let request = DS._payload packet
            question <- checkEither $ SV.getQuestion request
            flip IE.catchIOError (\ e -> putStrLn (show question) >> print e) $ do
                (ps, reply) <- timed $ maybe id SV.answerMinTtl answerMinTtl <$> W.query manager question
                putStrLn $ showQuestion question ++ printf " %.0f ms" (realToFrac ps * 1000 :: Double) -- debug
                let response = SV.applyReply reply request
                DS.sendTo socket packet { DS._payload = response }
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
        showQuestion q = show (D.qtype q) ++ " " ++ BSC.unpack (D.qname q)
