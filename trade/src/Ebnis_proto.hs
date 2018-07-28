{-# LANGUAGE RecordWildCards #-}

module Ebnis_proto (
    -- * ZLIB
    -- $zlib
    inflate
    , deflate
    -- * EBNIS binary coding of STOMP frames
    , Frame
    , Stomp.Command
    , Stomp.Header
    , Stomp.get_command
    , Stomp.get_headers
    -- ** Constructors
    , connected
    , Session
    -- ** Transput
    , decode
    , encode
    -- * Scramble
    , scramble
    -- * Tests
    , test
) where

import Prelude ()
import Meta.Prelude
import qualified Prelude as P

import qualified Codec.Zlib as Z
import qualified Control.Monad as M
import qualified Data.Array.IArray as Ar
import qualified Data.Array.ST.Safe as A
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Serialize as S

import qualified Meta.Crypto as Crypto
import qualified Meta.Stomp as Stomp

{- $zlib
We use zlib-bindings instead of zlib because zlib uses lazy ByteString and 'error'.
-}

inflate :: (MonadIO m) => ByteString -> m ByteString
inflate input = liftIO $ do
    inf <- Z.initInflate Z.defaultWindowBits
    ppr <- Z.feedInflate inf input
    cks <- collect ppr
    las <- Z.finishInflate inf
    return $ cks <> las

collect :: IO (Maybe ByteString) -> IO ByteString
collect popper = loop mempty
    where
        loop acc = do
            mbs <- popper
            case mbs of
                Just chunk -> loop (acc <> chunk)
                _ -> return acc

deflate :: (MonadIO m) => ByteString -> m ByteString
deflate input = liftIO $ do
    def <- Z.initDeflate compression_level Z.defaultWindowBits
    ck1 <- collect =<< Z.feedDeflate def input
    ck2 <- collect $ Z.finishDeflate def
    return $ ck1 <> ck2
    where
        compression_level = 9

type Frame = Stomp.Frame

type Session = Int

connected :: Session -> Frame
connected session = Stomp.mk_frame "CONNECTED" [("session", show session)]

decode :: (MonadIO m) => ByteString -> m Frame
decode compressed = do
    fail "not implemented"
    -- inflated <- inflate compressed
    -- either fail return $ S.runGet parser inflated

encode :: (MonadIO m) => Frame -> m ByteString
encode frame@Stomp.MkFrame{..} = do
    fail "not implemented"
    -- let bs_frame = S.runPut layout
    -- deflate bs_frame

type Session_key = ByteString

{- |
Reverse-engineered from @a.a.c.c:c(byte[])@.

The key

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
scramble :: (Monad m) => Session_key -> ByteString -> m ByteString
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
