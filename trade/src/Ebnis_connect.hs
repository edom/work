{-# LANGUAGE RecordWildCards #-}

module Ebnis_connect (
    Connection(..)
    , Party(..)
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

Reverse engineering notes for a.a.d.e:

* z = 1 means scramble client-to-server messages only.

* z = 2 means scramble both client-to-server and server-to-client messages.

* Other z means don't scramble.
-}
data Connection
    = MkConnection {
        _party :: Party -- ^ who we are running as
        , _version :: Word32
        , _login :: String
        , _passcode :: String
        , _scramble_ctos :: Bool
        , _scramble_stoc :: Bool
        , _session_key :: Session_key -- ^ v: symmetric cipher key for scramble
        , _bintext :: Word32 -- ^ A (should be 1): 1 = JSON, 2 = MsgPack
        , _compression :: Word32 -- ^ B (should be 2): 1 = an unknown compression algorithm, 2 = DEFLATE
    }
    deriving (Read, Show)

data Party
    = Client
    | Server
    deriving (Eq, Read, Show)

{- |
This 'def_connection' is for testing.
For reality, use 'decode'.
-}
def_connection :: Party -> Connection
def_connection party = MkConnection party 0 "" "" True False (fromString "session_key") 1 2

-- | Return a copy without sensitive data.
sanitize :: Connection -> Connection
sanitize con = con { _passcode = "<censored>", _session_key = fromString "<censored>" }

-- | Reverse-engineered from @a.a.d.h:a()@.
decode :: (Crypto.MonadRandom m) => Crypto.RSA_key_pair -> Net.Frame -> m Connection
decode key frame = do
    let payload = Net.get_payload frame
    dec <- Crypto.rsa_decrypt key payload
    jso <- either (\ msg -> fail $ "CONNECT: " ++ msg) return $ Json.decode dec
    case Json.assume_array jso of
        [j_version, j_login, j_passcode, jz, jv, ja, jb] -> case Json.assume_number j_version of
            version@0 -> case (Json.assume_number jz, Json.assume_number ja, Json.assume_number jb) of
                (_z@1, a@1, b@2) -> return $ MkConnection
                    Server
                    (P.round version)
                    (Json.assume_string j_login)
                    (Json.assume_string j_passcode)
                    True
                    False
                    (fromString $ Json.assume_string jv)
                    (P.round a)
                    (P.round b)
                _ -> fail $ "CONNECT: Invalid connection parameters: (z,A,B) = (" ++ show jz ++ ", " ++ show ja ++ ", " ++ show jb ++ ")"
            _ -> fail $ "CONNECT: Invalid version: " ++ show j_version
        _ -> fail "CONNECT: Invalid packet."

-- | Alias of 'decode'.
decode_connect :: (Crypto.MonadRandom m) => Crypto.RSA_key_pair -> Net.Frame -> m Connection
decode_connect = decode

encode :: (Crypto.MonadRandom m) => Crypto.RSA_public_key -> Connection -> m Net.Frame
encode key MkConnection{..} = do
    when (not $ _party == Client) $ do
        fail "Only clients can send a CONNECT frame"
    when (not $ _scramble_ctos && not _scramble_stoc) $ do
        fail "Unsupported scramble configuration"
    session_key <- Json.string _session_key
    let json = Json.array [
                Json.string $ show _version
                , Json.string _login
                , Json.string _passcode
                , Json.number (1 :: Word32)
                , session_key
                , Json.number _bintext
                , Json.number _compression
            ]
        plaintext = Json.encode json
    payload <- Crypto.rsa_encrypt key plaintext
    return $ Net.mk_frame payload

-- | Alias of 'encode'.
encode_connect :: (Crypto.MonadRandom m) => Crypto.RSA_public_key -> Connection -> m Net.Frame
encode_connect = encode
