{- |
We use zlib-bindings instead of zlib because zlib uses lazy ByteString and 'error'.
-}
module Meta.Zlib (
    deflate
    , inflate
) where

import Prelude ()
import Meta.Prelude

import qualified Codec.Zlib as Z

-- | Compress, shrink.
deflate :: (MonadIO m) => ByteString -> m ByteString
deflate input = liftIO $ do
    def <- Z.initDeflate compression_level Z.defaultWindowBits
    ck1 <- collect =<< Z.feedDeflate def input
    ck2 <- collect $ Z.finishDeflate def
    return $ ck1 <> ck2
    where
        compression_level = 9

{- |
Decompress, expand.

Security problem:
No length limit.
Decompression bomb.

https://security.stackexchange.com/questions/51071/zlib-deflate-decompression-bomb
-}
inflate :: (MonadIO m) => ByteString -> m ByteString
inflate input = liftIO $ do
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
