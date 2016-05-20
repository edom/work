{- |
This module should be imported qualified because some names clash with those in "Jvm_type".
-}
module Jvm_value
where

import qualified Data.ByteString as Bs

import Data.Int
    (
        Int8
        , Int16
        , Int32
        , Int64
    )
import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

-- * Value

-- | An inhabitant of this type is an element in the operand stack.
data Value
    = Padding -- ^ This should follow a Long and a Double, just to make the indexes right
    | Null -- ^ can we use Null for Padding?
    | Byte Int8 -- FIXME Word8 or Int8?
    | Short Int16
    | Integer Int32
    | Long Int64
    | Float Float
    | Double Double
    | Instance Class_name (Maybe Object)
    deriving (Read, Show, Eq)

type Class_name = Bs.ByteString

-- * Instance data

data Object
    = Mk_object
    {
        o_fields :: [(Bs.ByteString, Value)]
    }
    deriving (Read, Show, Eq)
