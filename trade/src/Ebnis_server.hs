{-# LANGUAGE FlexibleInstances #-}

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

import qualified Control.Monad.Reader as R

import qualified Ebnis_proto as Pro
import qualified Meta.Network as Net
import qualified Meta.Log as Log
import qualified Meta.Os as Os
import qualified Meta.Crypto as Crypto
import Trade_orphan ()

-- https://wiki.haskell.org/Implement_a_chat_server
-- http://hackage.haskell.org/package/network-2.7.0.2/docs/Network-Socket.html

-- | In practice this always instantiates to 'IO'.
class (Monad m) => Monad_server m where
    get_rsa_key_pair :: m Crypto.RSA_key_pair

data Server = MkServer {
        _rsa_key_pair :: Crypto.RSA_key_pair
    } deriving (Read, Show)

instance (Monad m) => Monad_server (R.ReaderT Server m) where
    get_rsa_key_pair = R.asks _rsa_key_pair


-- | Run the server.
main :: IO ()
main = do
    Net.withSocketsDo $ do
        -- TODO Make sure that only the user can access the key.
        rkp_file <- Os.getEnv "RSA_KEYPAIR_PEM_FILE"
        Log.log $ "Reading RSA key pair from \"" ++ rkp_file ++ "\"."
        rkp <- Crypto.rsa_read_key_pair_from_file rkp_file
        Log.log "Trying to listen on 0.0.0.0:62229."
        let server = MkServer rkp
        -- Problem: Unbounded forking.
        Net.serve Net.HostAny "62229" (handle server)

    where

        handle server socket addr = flip R.runReaderT server $ do
            Log.log $ "Accepted a connection from " ++ show addr ++ "."
            session <- login
            let session_id = 0 -- FIXME
            flip Pro.runReaderT session $ do
                Pro.write_stomp_frame $ Pro.connected session_id
                loop

            where

                loop = do
                    frame <- Pro.read_stomp_frame
                    loop

                login = do
                    rkp <- get_rsa_key_pair
                    con <- Pro.read_connect socket rkp
                    Log.log $ show $ Pro.sanitize con
                    return $ Pro.mk_session socket con
