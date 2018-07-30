{-# LANGUAGE FlexibleInstances #-}

module Ebnis_session (
    -- * Session
    Session
    , Session_id
    , Con.Session_key
    -- ** Construction
    , mk_session
    -- ** Monad
    , Monad_session(..)
    -- ** Things we can do with a session
    , deflate
    , inflate
    -- * Internal
    , MR.runReaderT
    -- Should be moved to Ebnis_scramble
    , scramble_0
    , unscramble_0
    -- Should be moved to Meta.Zlib
    -- ** ZLIB
    -- $zlib
    , zlib_inflate
    , zlib_deflate
    -- ** Tests
    , test
) where

import Prelude ()
import Meta.Prelude

import qualified Codec.Zlib as Z
import qualified Control.Monad as M
import qualified Control.Monad.Reader as MR
import qualified Data.Array.IArray as Ar
import qualified Data.Array.ST.Safe as A
import qualified Data.ByteString as B

import qualified Ebnis_connect as Con
import qualified Meta.Crypto as Crypto
import qualified Meta.Network as Net

type Session_id = Int

{- |
An inhabitant of this is a proof that the user has been successfully authenticated and authorized.
-}
data Session = MkSession {
        _socket :: Net.Socket
        , _connection :: Con.Connection
    } deriving (Show)

mk_session :: Net.Socket -> Con.Connection -> Session
mk_session = MkSession

-- | Things that depend on a 'Session'.
class (Functor m, Monad m) => Monad_session m where

    -- | See 'Session'.
    get_session :: m Session

    read_frame :: m Net.Payload

    write_frame :: Net.Payload -> m ()

    -- | See 'Con.Connection'.
    get_connection :: m Con.Connection
    get_connection = _connection <$> get_session

    -- | See 'Con.Session_key'.
    get_session_key :: m Con.Session_key
    get_session_key = Con._session_key <$> get_connection

    -- | See 'Con._compression'.
    get_compress :: m Word32
    get_compress = Con._compression <$> get_connection

    -- | See 'Con._scramble'.
    get_scramble :: m Word32
    get_scramble = Con._scramble <$> get_connection

    -- | See 'Con._bintext'.
    get_bintext :: m Word32
    get_bintext = Con._bintext <$> get_connection

    scramble :: ByteString -> m ByteString
    scramble bs = do
        method <- get_scramble
        case method of
            2 -> do
                key <- get_session_key
                scramble_0 key bs
            _ -> return bs

    unscramble :: ByteString -> m ByteString
    unscramble bs = do
        method <- get_scramble
        case method of
            2 -> do
                key <- get_session_key
                unscramble_0 key bs
            _ -> return bs

instance (Functor m, MonadIO m) => Monad_session (MR.ReaderT Session m) where
    get_session = MR.ask
    read_frame = get_socket >>= Net.read_frame
    write_frame frame = do
        socket <- get_socket
        Net.write_frame socket frame

get_socket :: (Monad_session m) => m Net.Socket
get_socket = _socket <$> get_session

deflate :: (MonadIO m, Monad_session m) => ByteString -> m ByteString
deflate bs = do
    method <- get_compress
    case method of
        2 -> zlib_deflate bs
        _ -> fail $ "Invalid compression method: " ++ show method

inflate :: (MonadIO m, Monad_session m) => ByteString -> m ByteString
inflate bs = do
    method <- get_compress
    case method of
        2 -> zlib_inflate bs
        _ -> fail $ "Invalid compression method: " ++ show method

{- $zlib
We use zlib-bindings instead of zlib because zlib uses lazy ByteString and 'error'.
-}

zlib_inflate :: (MonadIO m) => ByteString -> m ByteString
zlib_inflate input = liftIO $ do
    inf <- Z.initInflate Z.defaultWindowBits
    ppr <- Z.feedInflate inf input
    cks <- collect ppr
    las <- Z.finishInflate inf
    return $ cks <> las

-- | This exhausts a zlib popper.
collect :: IO (Maybe ByteString) -> IO ByteString
collect popper = loop mempty
    where
        loop acc = do
            mbs <- popper
            case mbs of
                Just chunk -> loop (acc <> chunk)
                _ -> return acc

zlib_deflate :: (MonadIO m) => ByteString -> m ByteString
zlib_deflate input = liftIO $ do
    def <- Z.initDeflate compression_level Z.defaultWindowBits
    ck1 <- collect =<< Z.feedDeflate def input
    ck2 <- collect $ Z.finishDeflate def
    return $ ck1 <> ck2
    where
        compression_level = 9

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
scramble_0 :: (Monad m) => Con.Session_key -> ByteString -> m ByteString
scramble_0 bs_key plaintext = do
    let key = B.unpack bs_key
    when (null key) $ fail "scramble_0: empty key"
    let ua_ciphertext = A.runSTUArray $ do
            a_key <- A.newListArray (0,255) (take 256 $ cycle key)
            a_plaintext <- A.newListArray (0, length plaintext - 1) (B.unpack plaintext)
            scramble_1 a_key a_plaintext
    return $ B.pack $ Ar.elems ua_ciphertext

type Table s = A.STUArray s Word8 Word8

{- |
@scramble_1 key plaintext@

The index of key must range from 0 to 255 inclusive.
The key must be 256 bytes long.
-}
scramble_1 :: Table s -> A.STUArray s Word Word8 -> ST s (A.STUArray s Word Word8)
scramble_1 key plaintext = do

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

-- | Inverse of 'scramble_0'.
unscramble_0 :: (Monad m) => Con.Session_key -> ByteString -> m ByteString
unscramble_0 = scramble_0

-- | Compare a scrambling with that of the original Java program.
test :: IO ()
test = do
    let k = fromString "This is the key."
        p = fromString "This is the plaintext."
        expected = fromString "OSybqniWnGCNymulrXZvSWGhIs/ZZA=="
    s <- scramble_0 k p
    putStr k >> putStr "\n"
    putStr p >> putStr "\n"
    let enc = Crypto.base64_encode s
    putStr enc >> putStr "\n"
    putStr $ show (expected == enc) ++ "\n"
