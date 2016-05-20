{- |
This module should be imported qualified because some names clash with those in "Jvm_value".
-}
module Jvm_type
where

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
    | Instance String -- ^ class name
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

type Return_type = Maybe Type

-- * Pretty-printing types

pretty_type :: Type -> String
pretty_type t = case t of
    Byte -> "byte"
    Short -> "short"
    Int -> "int"
    Long -> "long"
    Null -> "null"
    Array u -> pretty_type u ++ "[]"
    Instance c -> c
    _ -> show t

pretty_return_type :: Return_type -> String
pretty_return_type = maybe "void" pretty_type
