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
