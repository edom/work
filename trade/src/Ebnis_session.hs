{-# LANGUAGE FlexibleInstances #-}

module Ebnis_session (
    -- * Session
    Session
    , Session_id
    , Con.Session_key
    -- ** Construction
    , mk_session
    -- ** Monad
    , Monad_session(..)
    -- ** Things we can do with a session
    , deflate
    , inflate
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad.Reader as R

import qualified Ebnis_connect as Con
import qualified Ebnis_scramble as Scr
import qualified Meta.Network as Net
import qualified Meta.Zlib as Z

type Session_id = Word64

{- |
An inhabitant of this is a proof that the user has been successfully authenticated and authorized.
-}
data Session = MkSession {
        _socket :: Net.Socket
        , _connection :: Con.Connection
        , _session_id :: Session_id
    } deriving (Show)

mk_session :: Net.Socket -> Con.Connection -> Session_id -> Session
mk_session = MkSession

-- | Things that depend on a 'Session'.
class (Functor m, Monad m) => Monad_session m where

    -- | See 'Session'.
    get_session :: m Session

    read_frame :: m Net.Payload

    write_frame :: Net.Payload -> m ()

    get_session_id :: m Session_id
    get_session_id = _session_id <$> get_session

    -- | See 'Con.Connection'.
    get_connection :: m Con.Connection
    get_connection = _connection <$> get_session

    -- | See 'Con.Session_key'.
    get_session_key :: m Con.Session_key
    get_session_key = Con._session_key <$> get_connection

    -- | See 'Con._compression'.
    get_compress :: m Word32
    get_compress = Con._compression <$> get_connection

    -- | See 'Con._scramble'.
    get_scramble :: m Word32
    get_scramble = Con._scramble <$> get_connection

    -- | See 'Con._bintext'.
    get_bintext :: m Word32
    get_bintext = Con._bintext <$> get_connection

    scramble :: ByteString -> m ByteString
    scramble bs = do
        method <- get_scramble
        case method of
            2 -> do
                key <- get_session_key
                Scr.scramble key bs
            _ -> return bs

    unscramble :: ByteString -> m ByteString
    unscramble bs = do
        method <- get_scramble
        case method of
            2 -> do
                key <- get_session_key
                Scr.unscramble key bs
            _ -> return bs

instance (Functor m, MonadIO m) => Monad_session (R.ReaderT Session m) where
    get_session = R.ask
    read_frame = get_socket >>= Net.read_frame
    write_frame frame = do
        socket <- get_socket
        Net.write_frame socket frame

get_socket :: (Monad_session m) => m Net.Socket
get_socket = _socket <$> get_session

deflate :: (MonadIO m, Monad_session m) => ByteString -> m ByteString
deflate bs = do
    method <- get_compress
    case method of
        2 -> Z.deflate bs
        _ -> fail $ "Invalid compression method: " ++ show method

inflate :: (MonadIO m, Monad_session m) => ByteString -> m ByteString
inflate bs = do
    method <- get_compress
    case method of
        2 -> Z.inflate bs
        _ -> fail $ "Invalid compression method: " ++ show method
