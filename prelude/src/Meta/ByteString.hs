{-# LANGUAGE CPP #-}

module Meta.ByteString (
    B.ByteString
    , LazyByteString
    -- * Conversion
    -- $convert
    , toStrict
    , fromStrict
    -- * IO
    , C_putStr(..)
    , B.readFile
    , B.writeFile
) where

import Prelude hiding (putStr)

import Data.ByteString (ByteString)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

type LazyByteString = BL.ByteString

{- $convert
These functions are polyfilled on @bytestring <= 0.9@.
We don't know how efficient these polyfills are.

On @bytestring >= 0.10@, these functions are aliases of @toStrict@ and @fromStrict@ of "Data.ByteString.Lazy".
-}

toStrict :: LazyByteString -> ByteString
fromStrict :: ByteString -> LazyByteString

#if MIN_VERSION_bytestring(0,10,0)
toStrict = BL.toStrict
fromStrict = BL.fromStrict
#else
toStrict lbs = B.concat $ BL.toChunks lbs
fromStrict bs = BL.fromChunks [bs]
#endif

{- |
We should generalize this from IO to MonadIO.
-}
class C_putStr a where putStr :: a -> IO ()

instance C_putStr B.ByteString where putStr = B.putStr
instance C_putStr BL.ByteString where putStr = BL.putStr
