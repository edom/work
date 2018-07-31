{- |
Cryptography-related stuff, including non-cryptographic cryptography-related stuff.
-}
module Meta.Crypto (
    -- * Base64
    base64_encode
    , base64_decode
    , Base64_encoded_ByteString
    -- * RSA
    , Plaintext
    , Ciphertext
    -- ** Keys
    , RSA_public_key
    , RSA_key_pair
    -- ** Reading keys from files
    , PEM_file_path
    , rsa_read_public_key_from_file
    , rsa_read_key_pair_from_file
    -- ** Encryption and decryption
    , rsa_encrypt
    , rsa_decrypt
    -- * Monad
    , Ran.MonadRandom(..)
) where

import Prelude ()
import Meta.Prelude

import qualified Crypto.PubKey.RSA as CR
import qualified Crypto.PubKey.RSA.PKCS15 as CP
import qualified Crypto.Random as Ran
import qualified Data.ByteString.Base64 as Base64
import qualified OpenSSL as OS
import qualified OpenSSL.EVP.PKey as OE
import qualified OpenSSL.PEM as OP
import qualified OpenSSL.RSA as OR

type Base64_encoded_ByteString = ByteString

-- | See 'Base64.encode'.
base64_encode :: ByteString -> Base64_encoded_ByteString
base64_encode = Base64.encode

-- | See 'Base64.decode'.
base64_decode :: (Monad m) => Base64_encoded_ByteString -> m ByteString
base64_decode = either fail return . Base64.decode

-- | RSA public key.
type RSA_public_key = CR.PublicKey

-- | RSA key pair (private key and public key).
type RSA_key_pair = CR.PrivateKey

-- | Unencrypted message.
type Plaintext = ByteString

-- | Encrypted message.
type Ciphertext = ByteString

-- | Encrypt with RSA public key.
rsa_encrypt :: (Ran.MonadRandom m) => RSA_public_key -> Plaintext -> m Ciphertext
rsa_encrypt k p = CP.encrypt k p >>= either (fail . show) return

-- | Decrypt with RSA private key.
rsa_decrypt :: (Ran.MonadRandom m) => RSA_key_pair -> Ciphertext -> m Plaintext
rsa_decrypt k c = CP.decryptSafer k c >>= either (fail . show) return

type PEM_file_path = FilePath

-- | Read RSA public key from PEM file.
rsa_read_public_key_from_file :: PEM_file_path -> IO RSA_public_key
rsa_read_public_key_from_file path = OS.withOpenSSL $ do
    pem <- slurp path
    spk <- OP.readPublicKey pem
    osl <- maybe (user_error $ "Could not read RSA public key from file: " ++ path) return $ OE.toPublicKey spk
    return $ translate_pub (osl :: OR.RSAPubKey)

-- | Read RSA key pair from PEM file.
rsa_read_key_pair_from_file :: PEM_file_path -> IO RSA_key_pair
rsa_read_key_pair_from_file path = OS.withOpenSSL $ do
    pem <- slurp path
    skp <- OP.readPrivateKey pem OP.PwNone
    osl <- maybe (user_error $ "Could not read RSA key pair from file: " ++ path) return $ OE.toKeyPair skp
    translate_priv osl

-- | See 'translate_priv' for justification.
translate_pub :: (OR.RSAKey k) => k -> CR.PublicKey
translate_pub k = CR.PublicKey size n e
    where
        size = OR.rsaSize k
        n = OR.rsaN k
        e = OR.rsaE k

-- | HsOpenSSL can read PEM, but Cryptonite can't.
-- Cryptonite can decrypt RSA, but HsOpenSSL can't.
translate_priv :: (Monad m) => OR.RSAKeyPair -> m CR.PrivateKey
translate_priv k = do
    dP <- a "Missing rsaDMP1" $ OR.rsaDMP1 k
    dQ <- a "Missing rsaDMQ1" $ OR.rsaDMQ1 k
    qinv <- a "Missing rsaIQMP" $ OR.rsaIQMP k
    return $ CR.PrivateKey (translate_pub k) d p q dP dQ qinv
    where
        d = OR.rsaD k
        p = OR.rsaP k
        q = OR.rsaQ k
        a msg val = maybe (fail msg) return val
