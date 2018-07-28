module Ebnis_server (
    main
) where

import Prelude ()
import Meta.Prelude
import qualified Prelude as P

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Data.Serialize as Bin
import qualified Data.Time as Time
import qualified Network.Socket as S
import qualified Network.Simple.TCP as Tcp

import qualified Ebnis_connect as Con
import qualified Ebnis_proto as Pro
import qualified Meta.Os as Os
import qualified Meta.Crypto as Crypto

-- https://wiki.haskell.org/Implement_a_chat_server
-- http://hackage.haskell.org/package/network-2.7.0.2/docs/Network-Socket.html

class Log m where
    log :: String -> m ()

class (MonadIO m, Log m) => Serve m

instance Log IO where
    log msg = do
        utc <- Time.getCurrentTime
        tid <- C.myThreadId
        let time = Time.formatTime Time.defaultTimeLocale "%FT%T%03Q" utc
        putStrLn $ time ++ "\t[" ++ show tid ++ "]\t" ++ msg

instance Serve IO

main :: (Serve m) => m ()
main = do
    liftIO $ S.withSocketsDo $ do
        rkp_file <- Os.getEnv "RSA_KEYPAIR_PEM_FILE"
        log $ "Reading RSA key pair from \"" ++ rkp_file ++ "\"."
        rkp <- Crypto.rsa_read_key_pair_from_file rkp_file

        let
            handle socket addr = do
                log $ "Accepted a connection from " ++ show addr ++ "."
                login
                loop
                where

                    loop = do
                        frame <- read_frame
                        loop

                    login = do
                        frame <- read_frame
                        con <- Con.decode rkp frame
                        log $ show $ Con.sanitize con
                        let session = 0
                        write_frame $ Pro.connected session -- FIXME

                    -- | This returns the payload without the length.
                    read_frame :: IO ByteString
                    read_frame = do
                        b_len <- read_exactly 4
                        len <- either fail return $ Bin.runGet Bin.getWord32be b_len
                        let limit = 1048576
                        when (len > limit) $ fail $ "Frame too big: " ++ show len ++ " > " ++ show limit
                        read_exactly (fromIntegral len)

                    write_frame :: Pro.Frame -> IO ByteString
                    write_frame frame = do
                        fail "not implemented"

                    read_exactly :: Int -> IO ByteString
                    read_exactly n
                        | n <= 0 = return mempty
                        | otherwise = do
                            m_bs <- Tcp.recv socket n
                            case m_bs of
                                Just bs -> (bs <>) <$> read_exactly (n - length bs)
                                _ -> fail $ "Premature end of file. Expecting to read " ++ show n ++ " more bytes."

        log "Trying to listen on 0.0.0.0:62229."
        -- Problem: Unbounded forking.
        Tcp.serve Tcp.HostAny "62229" $ \ (socket, addr) -> handle socket addr
