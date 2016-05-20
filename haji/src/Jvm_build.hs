module Jvm_build
where

import Jvm_arch
    (
        Class(..)
        , S
        , Class_name
        , Method_name
    )
import qualified Jvm_arch as A
import qualified Jvm_type as T
import qualified Jvm_value as V

-- * Building a Class

new :: Class_name -> Class
new name =
    Mk_class
        {
            c_name = name
            , c_fields = []
            , c_methods = []
            , c_pool = []
            , c_static = []
        }

-- | Add native method.
native :: T.Return_type -> Method_name -> [T.Type] -> S V.Value -> Class -> Class
native ret nam args body cls =
    cls
        {
            c_methods = A.Mk_method 0 nam (T.Mk_signature args ret) (A.Native body) : c_methods cls
        }
