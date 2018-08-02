module Meta.Network (
    -- * Network.Socket
    S.Socket
    , read_exactly
    -- ** WSAStartup
    -- $windows
    , S.withSocketsDo
    -- * Network.Simple.TCP: Simple TCP client and server
    , connect
    , with_connect
    , serve
    , Tcp.send
    , Tcp.HostPreference(..)
    , Tcp.ServiceName
    , Tcp.SockAddr
    -- * Frame
    , Payload
    , read_frame
    , write_frame
) where

import Prelude ()
import Meta.Prelude

import qualified Data.Serialize as Bin
import qualified Network.Socket as S
import qualified Network.Simple.TCP as Tcp

{- $windows
Windows has questionable API design.
Why do we have to call WSAStartup?
-}

{- |
Read exactly n bytes, returning a ByteString of 'length' n; otherwise 'fail'.
-}
read_exactly :: S.Socket -> Int -> IO ByteString
read_exactly socket = loop
    where
        loop n
            | n <= 0 = return mempty
            | otherwise = do
                m_bs <- Tcp.recv socket n
                case m_bs of
                    Just bs -> (bs <>) <$> loop (n - length bs)
                    _ -> fail $ "Premature end of file. Expecting to read " ++ show n ++ " more bytes."

{- |
See 'Tcp.connectSock'.
-}
connect :: (MonadIO m) => Tcp.HostName -> Tcp.ServiceName -> m (S.Socket, S.SockAddr)
connect = Tcp.connectSock

-- | 'bracket'ed 'connect'.
with_connect :: Tcp.HostName -> Tcp.ServiceName -> (S.Socket -> S.SockAddr -> IO a) -> IO a
with_connect host port action =
    bracket (connect host port) (\ (sock, _) -> Tcp.closeSock sock) (uncurry action)

serve
    :: (MonadIO m)
    => Tcp.HostPreference -- ^ listen IP address
    -> Tcp.ServiceName -- ^ listen TCP port
    -> (S.Socket -> S.SockAddr -> IO ()) -- ^ what to do after accepting each connection
    -> m ()

serve host port handle = Tcp.serve host port (uncurry handle)

-- | Frame content, without length.
type Payload = ByteString

{- |
A frame is:

* 4-byte big-endian length N (of the payload), followed by

* N bytes of payload.

The function 'read_frame' returns the payload without the length.
-}
read_frame :: (MonadIO m) => S.Socket -> m Payload
read_frame socket = liftIO $ do
    b_len <- read_exactly socket 4
    len <- either fail return $ Bin.runGet Bin.getWord32be b_len
    let limit = 1048576
    when (len > limit) $ fail $ "Frame too big: " ++ show len ++ " > " ++ show limit
    read_exactly socket (fromIntegral len)

{- |
See 'read_frame' for the definition of \"frame\".

The function 'write_frame' writes the length of the payload and then the payload.
-}
write_frame :: (MonadIO m) => S.Socket -> Payload -> m ()
write_frame socket frame = liftIO $ do
    let len = length frame
        b_len = Bin.runPut $ Bin.putWord32be len
    Tcp.send socket b_len
