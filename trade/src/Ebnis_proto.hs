{-# LANGUAGE RecordWildCards #-}

module Ebnis_proto (
    -- * STOMP frames
    Frame
    , Stomp.Command
    , Stomp.Header
    , Stomp.get_command
    , Stomp.get_headers
    -- ** STOMP frame constructors
    , connected
    -- * EBNIS idiosyncrasies
    -- ** CONNECT special case
    , read_connect
    , Con.Connection
    , Con.sanitize
    , Con.decode_connect
    -- ** Binary coding of STOMP frames
    -- *** High-level
    , Ses.Monad_session(..)
    , Session_id
    , Ses.Session
    , Ses.mk_session
    , read_stomp_frame
    , write_stomp_frame
    -- *** Low-level
    , decode_stomp_frame
    , encode_stomp_frame
    , Ses.runReaderT
) where

import Prelude ()
import Meta.Prelude

import qualified Data.MessagePack as MP
import qualified Data.Serialize as S

import qualified Meta.Crypto as Crypto
import qualified Meta.Network as Net
import qualified Meta.Stomp as Stomp

import qualified Ebnis_connect as Con
import qualified Ebnis_session as Ses

type Frame = Stomp.Frame

read_stomp_frame :: (MonadIO m, Ses.Monad_session m) => m Stomp.Frame
read_stomp_frame = Ses.read_frame >>= decode_stomp_frame

write_stomp_frame :: (MonadIO m, Ses.Monad_session m) => Stomp.Frame -> m ()
write_stomp_frame = encode_stomp_frame >=> Ses.write_frame

type Session_id = Int

read_connect :: (MonadIO m, Crypto.MonadRandom m) => Net.Socket -> Crypto.RSA_key_pair -> m Con.Connection
read_connect socket rkp = do
    frame <- Net.read_frame socket
    Con.decode_connect rkp frame

connected :: Session_id -> Frame
connected session = Stomp.mk_frame "CONNECTED" [("session", show session)]

{- |
Don't use this for CONNECT.
Use 'read_connect' instead.

Reverse-engineered from @a.a.d.e:a(byte[])@.
-}
decode_stomp_frame :: (MonadIO m, Ses.Monad_session m) => ByteString -> m Frame
decode_stomp_frame compressed = do
    inflated <- Ses.inflate compressed
    unscrambled <- Ses.unscramble inflated
    mp <- either fail return $ S.runGet S.get unscrambled
    case mp of
        MP.ObjectArray (mp_cmd : params) | Just cmd <- as_int mp_cmd -> case (cmd, params) of
            (2, _) -> return $ Stomp.mk_frame "DISCONNECT" []
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
encode_stomp_frame :: (MonadIO m, Ses.Monad_session m) => Frame -> m ByteString
encode_stomp_frame frame@Stomp.MkFrame{..} = do
    msgpack_object <- MP.ObjectArray <$> case Stomp.get_command frame of
        "CONNECTED" -> do
            session <- Stomp.get_header "session" frame
            return [MP.ObjectInt 1, MP.ObjectString $ fromString session]
        command ->
            fail $ "Invalid STOMP command: " ++ command
    let bs_frame = S.runPut $ S.put msgpack_object
    scrambled <- Ses.scramble bs_frame
    deflated <- Ses.deflate scrambled
    return deflated
