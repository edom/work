module Jvm_type_system
where

import qualified Jvm_value as V
import qualified Jvm_type as T

import Jvm_value (Value)
import Jvm_type (Type)

type_of :: Value -> Type
type_of v = case v of
    V.Padding -> T.Void
    V.Null -> T.Null
    V.Bool _ -> T.Bool
    V.Char _ -> T.Char
    V.Byte _ -> T.Byte
    V.Short _ -> T.Short
    V.Integer _ -> T.Int
    V.Long _ -> T.Long
    V.Float _ -> T.Float
    V.Double _ -> T.Double
    V.Instance class_name _ -> T.Instance class_name
    V.Array t _ _ -> T.Array t

{- |
Default value for uninitialized fields.
-}
def_value :: Type -> Value
def_value t = case t of
    T.Void -> V.Padding
    T.Null -> V.Null
    T.Byte -> V.Byte 0
    T.Short -> V.Short 0
    T.Int -> V.Integer 0
    T.Long -> V.Long 0
    T.Float -> V.Float 0
    T.Double -> V.Double 0
    T.Array _ -> V.Null
    T.Instance _ -> V.Null
