module Jvm_constant
where

import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

import Data.ByteString (ByteString)

import qualified Control.Monad as Mo

import Jvm_member
    (
        Field_name
        , Method_name
    )

import qualified Jvm_arch as A
import qualified Jvm_type as T
import qualified List as Li

-- * Accessing constant pool entries

{- |
The index starts from one.
-}
get :: (A.Stateful m) => Word16 -> m A.Constant
get i = do
    frame <- A.get_frame
    let clas = A.f_class frame
    maybe
        (A.stop A.Invalid_constant_pool_index)
        return
        (A.c_pool clas `Li.at` (fromIntegral i - 1))

get_field_ref :: (A.Stateful m) => Word16 -> m (A.Class_name, Field_name, T.Type)
get_field_ref = get Mo.>=> want_field_ref

get_method_ref :: (A.Stateful m) => Word16 -> m (A.Class_name, Method_name, T.Signature)
get_method_ref = get Mo.>=> want_method_ref

get_class_name :: (A.Stateful m) => Word16 -> m A.Class_name
get_class_name = get Mo.>=> want_class_name

get_string :: (A.Stateful m) => Word16 -> m ByteString
get_string = get Mo.>=> want_string

want_string :: (A.Stateful m) => A.Constant -> m ByteString
want_string (A.C_string a) = return a
want_string _ = A.stop A.Expecting_string

want_class_name :: (A.Stateful m) => A.Constant -> m A.Class_name
want_class_name (A.C_class a) = return a
want_class_name _ = A.stop A.Expecting_class_name

want_field_ref :: (A.Stateful m) => A.Constant -> m (A.Class_name, Field_name, T.Type)
want_field_ref (A.C_fieldref a b c) = return (a, b, c)
want_field_ref _ = A.stop A.Expecting_fieldref

want_method_ref :: (A.Stateful m) => A.Constant -> m (A.Class_name, Method_name, T.Signature)
want_method_ref (A.C_methodref a b c) = return (a, b, c)
want_method_ref _ = A.stop A.Expecting_methodref
