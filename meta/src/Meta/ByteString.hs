module Meta.ByteString (
    B.ByteString
    , LazyByteString
    -- * Conversion
    , BL.toStrict
    , BL.fromStrict
    -- * IO
    , C_putStr(..)
) where

import Prelude hiding (putStr)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

type LazyByteString = BL.ByteString

{- |
We should generalize this from IO to MonadIO.
-}
class C_putStr a where putStr :: a -> IO ()

instance C_putStr B.ByteString where putStr = B.putStr
instance C_putStr BL.ByteString where putStr = BL.putStr
