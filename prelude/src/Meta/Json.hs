{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Meta.Json (
    -- * Value
    Value
    -- * Parsing
    , C_decode(..)
    , C_encode(..)
    -- * Value constructors
    , null
    , Number
    , C_number(..)
    , C_string(..)
    , Array(..)
    -- * Sloppy deconstruction
    , Key
    , at
    , assume_array
    , assume_string
    , assume_number
) where

import Prelude ()
import Meta.Prelude hiding (at, null)

import qualified Meta.ByteString as B
import qualified Meta.Text as T

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific as S
import qualified Data.Vector as V

{- |
JSON syntax tree.
See also 'A.Value' in "Data.Aeson".
-}
type Value = A.Value

type Number = S.Scientific

null :: Value
null = A.Null

class C_number a where
    number :: a -> Value

instance C_number S.Scientific where
    number = A.Number

instance C_number Word32 where
    number = A.toJSON

-- | @b@ is typically 'Value'.
class C_string a b where
    string :: a -> b

instance C_string String Value where
    string = A.String . T.convert_utf_8

instance (Monad m) => C_string B.ByteString (m Value) where
    string = fmap A.String . T.convert_utf_8

instance C_string T.Text Value where
    string = A.String

class Array a where
    array :: a -> Value

instance Array (V.Vector Value) where
    array = A.Array

instance Array [Value] where
    array = array . V.fromList

{- $parsing
The function 'A.decode' can be thought as having the type @'M.LazyByteString' -> 'Value'@.
-}
class C_decode a where decode :: a -> Either String Value
instance C_decode B.ByteString where decode = A.eitherDecodeStrict
instance C_decode B.LazyByteString where decode = A.eitherDecode

class C_encode a where
    encode :: Value -> a

instance C_encode B.LazyByteString where
    encode = A.encode

instance C_encode B.ByteString where
    encode = B.toStrict . encode

type Key = String

{- |
If the input Value is not of the suitable type for the key, then this returns 'null'.

If the key is not in the input, then this returns 'null'.
-}
class C_at key where
    at :: Value -> key -> Value
    infixl `at`

-- | Array.
instance C_at Int where
    at (A.Array a) key = maybe null id (a V.!? key)
    at _ _ = null

-- | Object.
instance C_at String where
    at val key = at val (T.pack key)

-- | Object.
instance C_at T.Text where
    at (A.Object hm) key = H.lookupDefault null key hm
    at _ _ = null

-- | This returns empty list on failure.
assume_array :: Value -> [Value]
assume_array (A.Array a) = V.toList a
assume_array _ = []

-- | This returns empty string on failure.
assume_string :: Value -> String
assume_string (A.String t) = T.unpack t
assume_string _ = ""

-- | This returns zero on failure.
assume_number :: Value -> Number
assume_number (A.Number a) = a
assume_number _ = 0
