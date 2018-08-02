{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Ebnis_session (
    -- * Session
    Session
    , Session_id
    , Con.Session_key
    -- ** Construction
    , mk_session
    -- ** Manipulation
    , set_session_id
    -- ** Monad
    , Monad_session(..)
    , Scrambled
    , get_payload
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

type Session_id = ByteString

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

set_session_id :: Session_id -> Session -> Session
set_session_id i s = s { _session_id = i }

newtype Scrambled a = MkScrambled a
    deriving (Read, Show)

get_payload :: Scrambled a -> a
get_payload (MkScrambled a) = a

-- | Things that depend on a 'Session'.
class (Functor m, Monad m) => Monad_session m where

    -- | See 'Session'.
    get_session :: m Session

    -- | Low level.
    read_frame :: m Net.Frame

    -- | Low level.
    write_frame :: Net.Frame -> m ()

    get_session_id :: m Session_id
    get_session_id = _session_id <$> get_session

    -- | See 'Con.Connection'.
    get_connection :: m Con.Connection
    get_connection = _connection <$> get_session

    -- | See 'Con.Session_key'.
    get_session_key :: m Con.Session_key
    get_session_key = Con._session_key <$> get_connection

    -- | See 'Con._party'.
    get_party :: m Con.Party
    get_party = Con._party <$> get_connection

    -- | See 'Con._compression'.
    get_compress :: m Word32
    get_compress = Con._compression <$> get_connection

    -- | This returns whether we want to scramble every frame we write.
    get_scramble_write :: m Bool
    get_scramble_write = do
        Con.MkConnection{..} <- get_connection
        return $
            (_party == Con.Client && _scramble_ctos)
            || (_party == Con.Server && _scramble_stoc)

    -- | This returns whether we need to unscramble every frame we read.
    get_unscramble_read :: m Bool
    get_unscramble_read = do
        Con.MkConnection{..} <- get_connection
        return $
            (_party == Con.Client && _scramble_stoc)
            || (_party == Con.Server && _scramble_ctos)

    -- | See 'Con._bintext'.
    get_bintext :: m Word32
    get_bintext = Con._bintext <$> get_connection

    scramble :: Net.Payload -> m (Scrambled Net.Payload)
    scramble bs = do
        necessary <- get_scramble_write
        MkScrambled <$> if necessary
            then do
                key <- get_session_key
                Scr.scramble key bs
            else return bs

    unscramble :: Scrambled Net.Payload -> m Net.Payload
    unscramble (MkScrambled bs) = do
        necessary <- get_unscramble_read
        if necessary
            then do
                key <- get_session_key
                Scr.unscramble key bs
            else return bs

instance (Functor m, MonadIO m) => Monad_session (R.ReaderT Session m) where
    get_session = R.ask
    read_frame = get_socket >>= Net.read_frame
    write_frame frame = do
        socket <- get_socket
        Net.write_frame socket frame

get_socket :: (Monad_session m) => m Net.Socket
get_socket = _socket <$> get_session

deflate :: (MonadIO m, Monad_session m) => Scrambled ByteString -> m Net.Frame
deflate (MkScrambled bs) = Net.mk_frame <$> do
    method <- get_compress
    case method of
        2 -> Z.deflate bs
        _ -> fail $ "Invalid compression method: " ++ show method

inflate :: (MonadIO m, Monad_session m) => Net.Frame -> m (Scrambled ByteString)
inflate frame = MkScrambled <$> do
    let payload = Net.get_payload frame
    method <- get_compress
    case method of
        2 -> Z.inflate payload
        _ -> fail $ "Invalid compression method: " ++ show method
