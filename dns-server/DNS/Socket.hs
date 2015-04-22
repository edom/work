module DNS.Socket
(
    newUdpSocket
    , recvFrom
    , sendTo
    , Packet(..)
)
where

import qualified System.IO.Error as IE

import qualified Data.ByteString.Lazy as BSL

import qualified Network.DNS as D

import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB

-- | Create a new UDP socket with SO_REUSEADDR socket option set.
newUdpSocket :: IO S.Socket
newUdpSocket = do
    socket <- S.socket S.AF_INET S.Datagram S.defaultProtocol
    S.setSocketOption socket S.ReuseAddr 1
    return socket

recvFrom :: S.Socket -> IO Packet
recvFrom socket = do
    (bsRequest, peerAddress) <- SB.recvFrom socket 512
    either (IE.ioError . IE.userError) (return . MkPacket peerAddress)
        $ D.decode $ BSL.fromStrict bsRequest

sendTo :: S.Socket -> Packet -> IO ()
sendTo socket packet =
    SB.sendAllTo socket (BSL.toStrict $ D.encode $ _payload packet) (_peer packet)

data Packet
    = MkPacket
    {
        _peer :: S.SockAddr
        , _payload :: D.DNSFormat
    }
    deriving (Show)
