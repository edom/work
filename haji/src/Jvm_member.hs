module Jvm_member
where

import Data.ByteString (ByteString)
import Jvm_type (Type)

data Field_ref
    = Mk_field_ref
    {
        fr_name :: Field_name
        , fr_type :: Type
    }
    deriving (Read, Show, Eq)

type Field_name = ByteString

type Method_name = ByteString
