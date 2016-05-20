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
import qualified GHC.Float as Gf

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

-- * Marshalling

-- What should the semantics be?
-- Should we be strict (not convert at all)?
-- Should we perform implicit widening?
-- Should we perform implicit conversion?
-- What is the relationship between from_java and to_java?
-- from_java (to_java x) =? x ???
-- to_java (from_java x) =? x ???
class From_java a where from_java :: Value -> Maybe a

instance From_java Int32 where
    from_java a = case a of
        -- Byte b -> Just (fromIntegral b) -- XXX sign- or zero- extend?
        Short b -> Just (fromIntegral b)
        Integer b -> Just b
        _ -> Nothing

instance From_java Int64 where
    from_java a = case a of
        Short b -> Just (fromIntegral b)
        Integer b -> Just (fromIntegral b)
        Long b -> Just b
        _ -> Nothing

instance From_java Float where
    from_java a = case a of
        Float b -> Just b
        _ -> Nothing

instance From_java Double where
    from_java a = case a of
        Float b -> Just (Gf.float2Double b)
        Double b -> Just b
        _ -> Nothing

class To_java a where to_java :: a -> Value

instance To_java Int8 where to_java = Byte
instance To_java Int16 where to_java = Short
instance To_java Int32 where to_java = Integer
instance To_java Int64 where to_java = Long
instance To_java Float where to_java = Float
instance To_java Double where to_java = Double
