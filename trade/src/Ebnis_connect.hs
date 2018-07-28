module Ebnis_connect (
    Connect(..)
    , sanitize
    , decode
) where

import Prelude ()
import Meta.Prelude
import qualified Prelude as A

import qualified Data.Serialize as S

import qualified Ebnis_proto as P
import qualified Meta.Crypto as Crypto
import qualified Meta.Json as Json

{- |
This contains sensitive data.
If you want to log this, 'sanitize' it first.
-}
data Connect
    = MkConnect {
        _version :: Word32
        , _login :: String
        , _passcode :: String
        , _param_z :: Word32 -- ^ z (should be 1)
        , _session_key :: String -- ^ v: symmetric cipher key
        , _encoding :: Word32 -- ^ A (should be 1): 1 = JSON, 2 = MsgPack
        , _compression :: Word32 -- ^ B (should be 2): 1 = an unknown compression algorithm, 2 = DEFLATE
    }
    deriving (Read, Show)

-- | Return a copy without sensitive data.
sanitize :: Connect -> Connect
sanitize con = con { _passcode = "<censored>", _session_key = "<censored>" }

-- | Reverse-engineered from @a.a.d.h:a()@.
decode :: (Crypto.MonadRandom m) => Crypto.RSA_key_pair -> ByteString -> m Connect
decode key bs = do
    dec <- Crypto.rsa_decrypt key bs
    jso <- either fail return $ Json.decode dec
    case Json.assume_array jso of
        [j_version, j_login, j_passcode, jz, jv, ja, jb] -> case Json.assume_number j_version of
            version@0 -> case (Json.assume_number jz, Json.assume_number ja, Json.assume_number jb) of
                (z@1, a@1, b@2) -> return $ MkConnect
                    (A.round version)
                    (Json.assume_string j_login)
                    (Json.assume_string j_passcode)
                    (A.round z)
                    (Json.assume_string jv)
                    (A.round a)
                    (A.round b)
                _ -> fail $ "CONNECT: Invalid connection parameters: (z,A,B) = (" ++ show jz ++ ", " ++ show ja ++ ", " ++ show jb ++ ")"
            _ -> fail $ "CONNECT: Invalid version: " ++ show j_version
        _ -> fail "CONNECT: Invalid packet."
