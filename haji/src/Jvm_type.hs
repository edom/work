{- |
This module should be imported qualified because some names clash with those in "Jvm_value".
-}
module Jvm_type
where

import Data.ByteString (ByteString)

import qualified Data.ByteString.UTF8 as Bu

-- * Type

data Type
    = Byte
    | Char
    | Double
    | Float
    | Int
    | Long
    | Void
    | Null
    | Instance ByteString -- ^ class name
    | Short
    | Bool
    | Array Type
    deriving (Read, Show, Eq)

data Signature
    = Mk_signature
    {
        s_arg_types :: [Type]
        , s_return_type :: Return_type
    }
    deriving (Read, Show, Eq)

type Return_type = Type

-- * Instance types

_Object :: Type
_Object = Instance (Bu.fromString "java/lang/Object")

_String :: Type
_String = Instance (Bu.fromString "java/lang/String")

-- * Pretty-printing types

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
