module Trade_main (
    ebnis_client
    , ebnis_server
    , test_rsa_round_trip
) where

import Prelude ()
import Meta.Prelude

import qualified Ebnis_client as C
import qualified Meta.Crypto as Crypto
import qualified Ebnis_server as S

-- | See 'C.main' in "Ebnis_client".
ebnis_client :: IO ()
ebnis_client = C.main

-- | See 'S.main' in "Ebnis_server".
ebnis_server :: IO ()
ebnis_server = S.main

test_rsa_round_trip :: IO ()
test_rsa_round_trip = do
    rkp <- S.read_key_pair
    cnf <- C.read_config
    let pub = C._server_public_key cnf
    putStr $ show rkp ++ "\n"
    putStr $ show cnf ++ "\n"
    ciphertext <- Crypto.rsa_encrypt pub plaintext
    decrypted <- Crypto.rsa_decrypt rkp ciphertext
    putStr $ show plaintext ++ "\n"
    putStr $ show decrypted ++ "\n"
    where
        plaintext :: ByteString
        plaintext = fromString "test"
