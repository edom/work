{-# LANGUAGE RecordWildCards #-}

{- |
Common things between server and client.
-}
module Ebnis_proto (
    -- * EBNIS idiosyncrasies
    -- ** CONNECT special case
    read_connect
    , Con.Connection
    , Con.sanitize
    , Con.decode_connect
    , Con.encode_connect
    -- * Message
    , Message(..)
    -- ** Constructors
    , connected
    , disconnect
    -- ** ByteString encoding
    , decode_message
    , encode_message
    -- ** Binary coding of STOMP frames
    -- *** High-level
    , Ses.Monad_session(..)
    , Ses.Session_id
    , Ses.Session
    , Ses.mk_session
    , read_message
    , write_message
    -- * Non-protocol-related reexports
    -- ** Meta.Network
    , Net.withSocketsDo
    , Net.Socket
    , Net.connect
    , Net.with_connect
    , Net.serve
    , Net.HostPreference(..)
    , Net.send
    -- ** Logging
    , Log.Monad_log(..)
    -- ** Base64
    , Crypto.base64_encode
    , Crypto.base64_decode
    -- ** Crypto
    , Crypto.RSA_key_pair
    , Crypto.RSA_public_key
    , Crypto.rsa_read_public_key_from_der_string
    , Crypto.rsa_read_public_key_from_file
    , Crypto.rsa_read_key_pair_from_file
    , Crypto.MonadRandom(..)
    -- ** Operating system
    , Os.getEnv
    -- ** Reader
    , R.MonadReader(..)
    , R.ReaderT
    , R.asks
    , R.runReaderT
    -- ** Monad
    , M.forever
    -- * STOMP frames
    , Stomp.Command
    , Stomp.Header
    , Stomp.get_command
    , Stomp.get_headers
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as M
import qualified Control.Monad.Reader as R
import qualified Data.MessagePack as MP
import qualified Data.Serialize as S

import qualified Meta.Crypto as Crypto
import qualified Meta.Log as Log
import qualified Meta.Network as Net
import qualified Meta.Os as Os
import qualified Meta.Stomp as Stomp

import qualified Ebnis_connect as Con
import qualified Ebnis_session as Ses

read_message :: (MonadIO m, Ses.Monad_session m) => m Message
read_message = Ses.read_frame >>= decode_message

write_message :: (MonadIO m, Ses.Monad_session m) => Message -> m ()
write_message = encode_message >=> Ses.write_frame

read_connect :: (MonadIO m, Crypto.MonadRandom m) => Net.Socket -> Crypto.RSA_key_pair -> m Con.Connection
read_connect socket rkp = do
    frame <- Net.read_frame socket
    Con.decode_connect rkp frame

data Message
    = Connected Ses.Session_id
    | Disconnect
    deriving (Read, Show)

connected :: Ses.Session_id -> Message
connected = Connected

disconnect :: Message
disconnect = Disconnect

{- |
Don't use this for CONNECT.
Use 'read_connect' instead.

Reverse-engineered from @a.a.d.e:a(byte[])@.
-}
decode_message :: (MonadIO m, Ses.Monad_session m) => ByteString -> m Message
decode_message compressed = do
    inflated <- Ses.inflate compressed
    unscrambled <- Ses.unscramble inflated
    mp <- either fail return $ S.runGet S.get unscrambled
    case mp of
        MP.ObjectArray (mp_cmd : params) | Just cmd <- as_int mp_cmd -> case (cmd, params) of
            (2, _) -> return Disconnect
            _ -> fail $ "Invalid MessagePack object: " ++ show mp
        _ -> fail $ "Invalid MessagePack object: " ++ show mp
    where
        as_int :: (Monad m) => MP.Object -> m Int
        as_int obj = case obj of
            MP.ObjectUInt a -> return $ fromIntegral a
            MP.ObjectInt a -> return $ fromIntegral a
            _ -> fail $ "as_int: " ++ show obj

{- |
Don't use this for CONNECT.

Reverse-engineered from @a.a.d.h:a()@.
-}
encode_message :: (MonadIO m, Ses.Monad_session m) => Message -> m ByteString
encode_message msg = do
    msgpack_object <- MP.ObjectArray <$> case msg of
        Connected session_id -> do
            return [MP.ObjectInt 1, MP.ObjectString $ fromString $ show session_id]
        _ ->
            fail $ "Invalid message: " ++ show msg
    let bs_frame = S.runPut $ S.put msgpack_object
    scrambled <- Ses.scramble bs_frame
    deflated <- Ses.deflate scrambled
    return deflated
