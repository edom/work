{-# LANGUAGE RecordWildCards #-}

module Ebnis_connect (
    Connection(..)
    , def_connection
    , Session_key
    , sanitize
    , decode
    , encode
    , decode_connect
    , encode_connect
) where

import Prelude ()
import Meta.Prelude
import qualified Prelude as P

import qualified Meta.Crypto as Crypto
import qualified Meta.Json as Json
import qualified Meta.Network as Net

{- |
Key for a symmetric cipher that is used to encrypt the session.

The key length must not exceed 256 bytes.
-}
type Session_key = ByteString

{- |
This contains sensitive data.
If you want to log this, 'sanitize' it first.
-}
data Connection
    = MkConnection {
        _version :: Word32
        , _login :: String
        , _passcode :: String
        , _scramble :: Word32 -- ^ z: 2 = scramble, other = don't scramble
        , _session_key :: Session_key -- ^ v: symmetric cipher key for scramble
        , _bintext :: Word32 -- ^ A (should be 1): 1 = JSON, 2 = MsgPack
        , _compression :: Word32 -- ^ B (should be 2): 1 = an unknown compression algorithm, 2 = DEFLATE
    }
    deriving (Read, Show)

{- |
This 'def_connection' is for testing.
For reality, use 'decode'.
-}
def_connection :: Connection
def_connection = MkConnection 0 "" "" 1 (fromString "session_key") 1 2

-- | Return a copy without sensitive data.
sanitize :: Connection -> Connection
sanitize con = con { _passcode = "<censored>", _session_key = fromString "<censored>" }

-- | Reverse-engineered from @a.a.d.h:a()@.
decode :: (Crypto.MonadRandom m) => Crypto.RSA_key_pair -> Net.Payload -> m Connection
decode key bs = do
    dec <- Crypto.rsa_decrypt key bs
    jso <- either fail return $ Json.decode dec
    case Json.assume_array jso of
        [j_version, j_login, j_passcode, jz, jv, ja, jb] -> case Json.assume_number j_version of
            version@0 -> case (Json.assume_number jz, Json.assume_number ja, Json.assume_number jb) of
                (z@1, a@1, b@2) -> return $ MkConnection
                    (P.round version)
                    (Json.assume_string j_login)
                    (Json.assume_string j_passcode)
                    (P.round z)
                    (fromString $ Json.assume_string jv)
                    (P.round a)
                    (P.round b)
                _ -> fail $ "CONNECT: Invalid connection parameters: (z,A,B) = (" ++ show jz ++ ", " ++ show ja ++ ", " ++ show jb ++ ")"
            _ -> fail $ "CONNECT: Invalid version: " ++ show j_version
        _ -> fail "CONNECT: Invalid packet."

-- | Alias of 'decode'.
decode_connect :: (Crypto.MonadRandom m) => Crypto.RSA_key_pair -> Net.Payload -> m Connection
decode_connect = decode

encode :: (Crypto.MonadRandom m) => Crypto.RSA_public_key -> Connection -> m Net.Payload
encode key MkConnection{..} = do
    session_key <- Json.string _session_key
    let json = Json.array [
                Json.string $ show _version
                , Json.string _login
                , Json.string _passcode
                , Json.number _scramble
                , session_key
                , Json.number _bintext
                , Json.number _compression
            ]
        bs = Json.encode json
    Crypto.rsa_encrypt key bs

-- | Alias of 'encode'.
encode_connect :: (Crypto.MonadRandom m) => Crypto.RSA_public_key -> Connection -> m Net.Payload
encode_connect = encode
