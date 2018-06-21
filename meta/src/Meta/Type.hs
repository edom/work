module Meta.Type where

import qualified Meta.Cbp_internal as C
import qualified Meta.HsType as H
import qualified Meta.JavaType as J
import qualified Meta.SqlType as S

cbp_to_java :: C.Type -> J.Type
cbp_to_java typ = case typ of
    C.TBool -> J.Boolean
    C.TInt32 -> J.Int32
    C.TInt64 -> J.Int64
    C.TString -> J.string
    C.TDecimal -> J.decimal
    _ -> error $ "Meta.Type.cbp_to_java: not implemented: " ++ show typ

data_to_hs :: S.Type -> H.Type
data_to_hs typ = case typ of
    S.Int32 -> H.int32
    S.Int64 -> H.int64
    S.VarChar _ -> H.string
    _ -> error $ "Meta.Type.data_to_hs: not implemented: " ++ show typ

data_to_cbp :: S.Type -> C.Type
data_to_cbp typ = case typ of
    S.Boolean -> C.TBool
    S.Int32 -> C.TInt32
    S.Int64 -> C.TInt64
    S.VarChar _ -> C.TString
    S.Numeric _ _ -> C.TDecimal
    _ -> error $ "Meta.Type.data_to_cbp: not implemented: " ++ show typ
