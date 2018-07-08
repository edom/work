module Meta.JvmMember (
    Field_ref(..)
    , Field_name
    , Method_name
) where

import Data.ByteString (ByteString)
import Meta.JvmType (Type)

data Field_ref
    = Mk_field_ref
    {
        fr_name :: Field_name
        , fr_type :: Type
    }
    deriving (Read, Show, Eq)

type Field_name = ByteString

type Method_name = ByteString
