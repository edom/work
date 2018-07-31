{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{- |
The plan:

* "Ebnis_server" builds on "Ebnis_proto_7".

* "Ebnis_proto_7" (application: STOMP) builds on "Ebnis_proto_6".

* "Ebnis_proto_6" (presentation: compression and encryption) builds on "Ebnis_proto_5".

* "Ebnis_proto_5" (session: connect, @Monad_session@) builds on "Meta.Network".

* "Meta.Network" (framing) builds on TCP (OSI layer 4).

* TCP builds on IP (OSI layer 3).
-}
module Ebnis_server (
    main
    , Monad_server(..)
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Concurrent.STM as C

import qualified Ebnis_proto as P
import Trade_orphan ()

data Server = MkServer {
        _rsa_key_pair :: P.RSA_key_pair
        , _tv_session_id :: C.TVar P.Session_id
    }

-- | In practice this always instantiates to 'IO'.
class (Monad m) => Monad_server m where

    get_rsa_key_pair :: m P.RSA_key_pair

    -- | Increment.
    get_next_session_id :: m P.Session_id

instance (MonadIO m) => Monad_server (P.ReaderT Server m) where
    get_rsa_key_pair = _rsa_key_pair <$> P.ask
    get_next_session_id = do
        MkServer{..} <- P.ask
        liftIO $ C.atomically $ do
            x <- C.readTVar _tv_session_id
            C.modifyTVar' _tv_session_id (+ 1)
            return x

-- https://wiki.haskell.org/Implement_a_chat_server
-- http://hackage.haskell.org/package/network-2.7.0.2/docs/Network-Socket.html

-- | Run the server.
main :: IO ()
main = do
    P.withSocketsDo $ do
        tv_session_id <- C.newTVarIO (0 :: P.Session_id)
        rkp <- read_key
        P.log "Trying to listen on 0.0.0.0:62229."
        let server = MkServer rkp tv_session_id
        -- Problem: Unbounded forking.
        P.serve P.HostAny "62229" (handle server)

    where

        -- TODO Make sure that only the user can access the key.
        read_key = do
            path <- P.getEnv "RSA_KEYPAIR_PEM_FILE"
            P.log $ "Reading RSA key pair from \"" ++ path ++ "\"."
            P.rsa_read_key_pair_from_file path

        -- This services one client connection.
        handle server socket addr = do
            P.log $ "Accepted a connection from " ++ show addr ++ "."
            session <- runReaderT server $ login socket
            runReaderT session $ do
                session_id <- P.get_session_id
                P.write_message $ P.connected session_id
                P.forever $ do
                    msg <- P.read_message
                    return ()

        runReaderT = flip P.runReaderT

        login socket = do
            rkp <- get_rsa_key_pair
            con <- P.read_connect socket rkp
            -- TODO authenticate
            session_id <- get_next_session_id
            P.log $ "Authenticated: " ++ show (P.sanitize con)
            return $ P.mk_session socket con session_id
