{- |
This module should be imported qualified because some names clash with those in "Jvm_type".
-}
module Jvm_value
where

import qualified Data.Char as Ch
import qualified Data.List as List

import qualified Data.ByteString as Bs

import qualified Data.ByteString.UTF8 as Bu

import Data.IORef (IORef, readIORef)
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

import Jvm_member
    (
        Field_ref
    )

import qualified Jvm_type as T

-- * Value

{- |
An inhabitant of this type represents things that Java variables can hold.

From execution point of view, an inhabitant of this type represents
an element of an operand stack or of a local variable array.
-}
data Value
    = Padding -- ^ This should follow a Long and a Double, just to make the indexes right
    | Null -- ^ can we use Null for Padding?
    | Bool Word8
    | Char Word16 -- ^ Java char is 16-bit unsigned integer
    | Byte Int8 -- FIXME Word8 or Int8?
    | Short Int16
    | Integer Int32
    | Long Int64
    | Float Float
    | Double Double
    | Instance Class_name (IORef [(Field_ref, Value)]) -- ^ embrace the 'IO' for instance field values
    | Array T.Type Int32 (IORef [Value])

type Class_name = Bs.ByteString

-- TODO use ForeignPtr for Array?

-- * Pretty-printing

{- |
If the input is an instance, this does not read its field values.
-}
pretty :: Value -> String
pretty v = case v of
    Padding -> "<padding>"
    Null -> "null"
    Byte a -> "byte " ++ show a
    Bool a -> "bool " ++ show a
    Char a -> "char " ++ show a
    Short a -> "short " ++ show a
    Integer a -> "int " ++ show a
    Long a -> "long " ++ show a
    Float a -> "float " ++ show a
    Double a -> "double " ++ show a
    Instance c _ -> "instanceof " ++ Bu.toString c
    Array t n _ -> T.pretty t ++ "[" ++ show n ++ "]"

{- |
If the input is an instance, this also reads its field values.

This returns the string; this does not print the string yet.
-}
pretty_io :: Value -> IO String
pretty_io v = case v of
    Array T.Char n r -> do
        content <- string_from_jchar_list <$> readIORef r
        return $ pretty v ++ " " ++ show content
    Array t n r -> do
        content <- readIORef r
        texts <- mapM pretty_io content
        return $ pretty v ++ " [" ++ List.intercalate ", " texts ++ "]"
    _ -> return (pretty v)

-- * Marshalling strings

-- XXX This may truncate the Int?
-- XXX This is grossly inefficient?
jchar_list_from_string :: String -> [Value]
jchar_list_from_string = map (Char . fromIntegral . Ch.ord)

string_from_jchar_list :: [Value] -> String
string_from_jchar_list = map (Ch.chr . unchar)
    where
        unchar (Char x) = fromIntegral x
        unchar _ = 0 -- XXX

-- * Checking values

{- |
A value is a category-2 value iff it takes 8 bytes.
-}
is_category_2 :: Value -> Bool
is_category_2 v = case v of
    Long _ -> True
    Double _ -> True
    _ -> False

{- |
Only 'Null' satisfies this predicate.
-}
is_null :: Value -> Bool
is_null Null = True
is_null _ = False

-- * Converting Java 'Value's to Haskell values

{- $
For the other way around (converting Haskell values to Java 'Value's),
just use the 'Value' constructors ('Integer', 'Long', and so on).
-}

integer :: Value -> Maybe Int32
integer (Integer x) = return x
integer v = Nothing

long :: Value -> Maybe Int64
long (Long x) = return x
long v = Nothing

float :: Value -> Maybe Float
float (Float x) = return x
float v = Nothing

double :: Value -> Maybe Double
double (Double x) = return x
double v = Nothing

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

{-
class Marshal m a where
    from_java :: m a
    to_java :: a -> m Value
-}
