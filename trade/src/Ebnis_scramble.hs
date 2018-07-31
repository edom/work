{- |
Likely-unsecure home-grown symmetric cipher.
-}
module Ebnis_scramble (
    -- * Encrypt
    scramble
    -- * Decrypt
    , unscramble
    -- * Tests
    , test
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as M
import qualified Data.Array.IArray as Ar
import qualified Data.Array.ST.Safe as A
import qualified Data.ByteString as B

import qualified Ebnis_connect as Con
import qualified Meta.Crypto as Crypto

{- |
Reverse-engineered from @a.a.c.c:c(byte[])@.

The key cannot be longer than 256 bytes.

Security notes:

This seems to be a home-grown cipher.

It's /reciprocal/: the encryption function and the decryption function are the same: @encrypt = decrypt@.
Thus the cipher is an involution: @encrypt (encrypt x) = x@.

It's a permutation cipher.

It's a stream cipher.

It's likely unsecure.
It doesn't mix the ciphertext.
If two plaintexts share a prefix, then their ciphertexts also share a prefix if encrypted with the same key.
-}
scramble :: (Monad m) => Con.Session_key -> ByteString -> m ByteString
scramble bs_key plaintext = do
    let key = B.unpack bs_key
    when (null key) $ fail "scramble: empty key"
    let ua_ciphertext = A.runSTUArray $ do
            a_key <- A.newListArray (0,255) (take 256 $ cycle key)
            a_plaintext <- A.newListArray (0, length plaintext - 1) (B.unpack plaintext)
            do_scramble a_key a_plaintext
    return $ B.pack $ Ar.elems ua_ciphertext

type Table s = A.STUArray s Word8 Word8

{- |
@do_scramble key plaintext@

The index of key must range from 0 to 255 inclusive.
The key must be 256 bytes long.
-}
do_scramble :: Table s -> A.STUArray s Word Word8 -> ST s (A.STUArray s Word Word8)
do_scramble key plaintext = do

    -- Initialize table to the identity function.
    let indexes = [0..255]
    table <- A.newListArray (0,255) indexes :: ST s (Table s)

    let swap a b = do
            x <- A.readArray table a
            y <- A.readArray table b
            A.writeArray table a y
            A.writeArray table b x

        init a i = do
            ki <- A.readArray key i
            ti <- A.readArray table i
            let b = ki + ti + a
            swap i b
            return b

    M.foldM_ init 0 indexes

    bounds <- A.getBounds plaintext
    ciphertext <- A.newArray bounds 0

    let stream a i = do
            let j = fromIntegral i + 1
            tj <- A.readArray table j
            let b = tj + a
            tb <- A.readArray table b
            let k = tb + tj
            swap j b
            pi <- A.readArray plaintext i
            tk <- A.readArray table k
            A.writeArray ciphertext i (pi `xor` tk)
            return b

    M.foldM_ stream 0 (A.range bounds)
    return ciphertext

-- | Inverse of 'scramble'.
unscramble :: (Monad m) => Con.Session_key -> ByteString -> m ByteString
unscramble = scramble

-- | Compare a scrambling with that of the original Java program.
test :: IO ()
test = do
    let k = fromString "This is the key."
        p = fromString "This is the plaintext."
        expected = fromString "OSybqniWnGCNymulrXZvSWGhIs/ZZA=="
    s <- scramble k p
    putStr k >> putStr "\n"
    putStr p >> putStr "\n"
    let enc = Crypto.base64_encode s
    putStr enc >> putStr "\n"
    putStr $ show (expected == enc) ++ "\n"
