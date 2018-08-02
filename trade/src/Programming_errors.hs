module Programming_errors where

import Data.String (fromString)

import qualified Data.ByteString.Base64 as Base64
import qualified Meta.ByteString as B

import qualified OpenSSL.DER as OD
import qualified OpenSSL.RSA as OR

{- |
The input is in DER form.
-}
rsa_read_public_key_from_der_string :: (Monad m) => B.ByteString -> m OR.RSAPubKey
rsa_read_public_key_from_der_string der = do
    maybe (fail "Invalid DER string") return $ OD.fromDERPub der

{-
This replicates the error in 'rsa_read_public_key_from_der_string'.

Using HsOpenSSL doesn't work.
-}
error_0 :: IO ()
error_0 = do
    let str = "MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDVd/gb2ORdLI7nTRHJR8C5EHs4RkRBcQuQdHkZ6eq0xnV2f0hkWC8h0mYH/bmelb5ribwulMwzFkuktXoufqzoft6Q6jLQRnkNJGRP6yA4bXqXfKYj1yeMusIPyIb3CTJT/gfZ40oli6szwu4DoFs66IZpJLv4qxU9hqu6NtJ+8QIDAQAB"
    let bas = fromString str
    der <- either fail return $ Base64.decode bas
    -- But this works: @openssl -inform der -in what.der@
    B.writeFile "what.der" der
    rpk <- rsa_read_public_key_from_der_string der
    putStr $ show rpk
