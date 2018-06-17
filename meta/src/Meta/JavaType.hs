module Meta.JavaType where

import qualified Data.List as L

-- | Qualified name.
type QName = String

data Type
    = Void
    | Boolean
    | Char
    | Int8 -- ^ two's-complement 8-bit signed integer primitive type
    | Int16 -- ^ two's-complement 16-bit signed integer primitive type
    | Int32 -- ^ two's-complement 32-bit signed integer primitive type
    | Int64 -- ^ two's-complement 64-bit signed integer primitive type
    | Array Type -- ^ array reference type
    | Ref QName -- ^ other reference type
    | Apply Type [Type] -- ^ applied reference type; each parameter should be a reference type
    deriving (Read, Show)

-- * Rendering

render :: Type -> String
render typ = case typ of
    Void -> "void"
    Boolean -> "boolean"
    Char -> "char"
    Int8 -> "byte"
    Int16 -> "short"
    Int32 -> "int"
    Int64 -> "long"
    Array ty -> render ty ++ "[]"
    Ref qname -> qname
    Apply ty pars -> render ty ++ "<" ++ L.intercalate ", " (map render pars) ++ ">"

-- * Predefined types

void :: Type
void = Void

primBool :: Type
primBool = Boolean

primInt8 :: Type
primInt8 = Int8

primInt16 :: Type
primInt16 = Int16

primInt32 :: Type
primInt32 = Int32

primInt64 :: Type
primInt64 = Int64

boxBool :: Type
boxBool = Ref "java.lang.Boolean"

boxInt8 :: Type
boxInt8 = Ref "java.lang.Byte"

boxInt16 :: Type
boxInt16 = Ref "java.lang.Short"

boxInt32 :: Type
boxInt32 = Ref "java.lang.Integer"

boxInt64 :: Type
boxInt64 = Ref "java.lang.Long"

string :: Type
string = Ref "java.lang.String"

inputStream :: Type
inputStream = Ref "java.io.InputStream"

outputStream :: Type
outputStream = Ref "java.io.OutputStream"

printWriter :: Type
printWriter = Ref "java.io.PrintWriter"

dataSource :: Type
dataSource = Ref "javax.sql.DataSource"

ref :: String -> Type
ref qname = Ref qname
