module Meta.Crypto (
    base64_encode
    -- * RSA
    , RSA_key_pair
    , rsa_decrypt
    , rsa_read_key_pair_from_file
    -- * Monad
    , Ran.MonadRandom
) where

import Prelude ()
import Meta.Prelude

import qualified Crypto.PubKey.RSA as CRSA
import qualified Crypto.PubKey.RSA.PKCS15 as Decrypt
import qualified Crypto.Random as Ran
import qualified Data.ByteString.Base64 as Base64
import qualified OpenSSL as OSSL
import qualified OpenSSL.EVP.PKey as PK
import qualified OpenSSL.PEM as PEM
import qualified OpenSSL.RSA as RSA

base64_encode :: ByteString -> ByteString
base64_encode = Base64.encode

type RSA_key_pair = CRSA.PrivateKey

rsa_decrypt :: (Ran.MonadRandom m) => RSA_key_pair -> ByteString -> m ByteString
rsa_decrypt k c = Decrypt.decryptSafer k c >>= either (fail . show) return

rsa_read_key_pair_from_file :: FilePath -> IO RSA_key_pair
rsa_read_key_pair_from_file path = OSSL.withOpenSSL $ do
    pem <- slurp path
    skp <- PEM.readPrivateKey pem PEM.PwNone
    osl <- maybe (user_error $ "Could not read RSA key pair from file: " ++ path) return $ PK.toKeyPair skp
    translate osl

-- | HsOpenSSL can read PEM, but Cryptonite can't.
-- Cryptonite can decrypt RSA, but HsOpenSSL can't.
translate :: (Monad m) => RSA.RSAKeyPair -> m CRSA.PrivateKey
translate k = do
    dP <- a "Missing rsaDMP1" $ RSA.rsaDMP1 k
    dQ <- a "Missing rsaDMQ1" $ RSA.rsaDMQ1 k
    qinv <- a "Missing rsaIQMP" $ RSA.rsaIQMP k
    return $ CRSA.PrivateKey pub d p q dP dQ qinv
    where
        pub = CRSA.PublicKey size n e
        d = RSA.rsaD k
        p = RSA.rsaP k
        q = RSA.rsaQ k
        size = RSA.rsaSize k
        n = RSA.rsaN k
        e = RSA.rsaE k
        a msg val = maybe (fail msg) return val
