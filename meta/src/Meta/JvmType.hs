module Meta.JvmType (
    -- * Type
    Type(..)
    , Signature(..)
    , Return_type
    -- * Instance types
    , _Object
    , _String
    -- * Pretty-printing types
    , pretty
) where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as Bu

data Type
    = Byte
    | Char
    | Double
    | Float
    | Int
    | Long
    | Void
    | Null
    | Instance Bs.ByteString -- ^ class name
    | Short
    | Bool
    | Array Type
    deriving (Read, Show, Eq, Ord)

data Signature
    = Mk_signature
    {
        s_arg_types :: [Type]
        , s_return_type :: Return_type
    }
    deriving (Read, Show, Eq)

type Return_type = Type

_Object :: Type
_Object = Instance (Bu.fromString "java/lang/Object")

_String :: Type
_String = Instance (Bu.fromString "java/lang/String")

pretty :: Type -> String
pretty t = case t of
    Byte -> "byte"
    Char -> "char"
    Double -> "double"
    Float -> "float"
    Short -> "short"
    Int -> "int"
    Long -> "long"
    Void -> "void"
    Null -> "null"
    Bool -> "boolean"
    Array u -> pretty u ++ "[]"
    Instance c -> Bu.toString c
