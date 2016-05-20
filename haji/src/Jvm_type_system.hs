module Jvm_type_system
where

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_value as V
import qualified Jvm_type as T

import Jvm_value (Value)
import Jvm_type (Type)

type_of :: Value -> Type
type_of v = case v of
    V.Padding -> T.Void
    V.Null -> T.Null
    V.Byte _ -> T.Byte
    V.Short _ -> T.Short
    V.Integer _ -> T.Int
    V.Long _ -> T.Long
    V.Float _ -> T.Float
    V.Double _ -> T.Double
    V.Instance class_name _ -> T.Instance (Bu.toString class_name)
