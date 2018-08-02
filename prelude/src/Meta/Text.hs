module Meta.Text (
    -- * Type
    T.Text
    -- * Conversion from and to String
    , T.pack
    , T.unpack
    -- * Coding from and to ByteString
    , E.decodeUtf8'
    , E.encodeUtf8
    -- * UTF-8: String, ByteString, Text
    , C_convert_utf_8(..)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

class C_convert_utf_8 a b where
    convert_utf_8 :: a -> b

instance C_convert_utf_8 B.ByteString String where
    convert_utf_8 = BU.toString

instance (Monad m) => C_convert_utf_8 B.ByteString (m T.Text) where
    convert_utf_8 = either (fail . show) return . E.decodeUtf8'

instance C_convert_utf_8 String B.ByteString where
    convert_utf_8 = BU.fromString

instance C_convert_utf_8 String T.Text where
    convert_utf_8 = T.pack

instance C_convert_utf_8 T.Text String where
    convert_utf_8 = T.unpack
