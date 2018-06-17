module Meta.Type where

import qualified Meta.Cbp_internal as C
import qualified Meta.Data_internal as D
import qualified Meta.HsType as H
import qualified Meta.JavaType as J

cbp_to_java :: C.Type -> J.Type
cbp_to_java typ = case typ of
    C.TInt32 -> J.Int32
    C.TInt64 -> J.Int64
    C.TString -> J.string
    _ -> error $ "Meta.Type.cbp_to_java: not implemented: " ++ show typ

data_to_hs :: D.Type -> H.Type
data_to_hs typ = case typ of
    D.TInt32 -> H.int32
    D.TInt64 -> H.int64
    D.TVarChar _ -> H.string
    _ -> error $ "Meta.Type.data_to_hs: not implemented: " ++ show typ

data_to_cbp :: D.Type -> C.Type
data_to_cbp typ = case typ of
    D.TInt32 -> C.TInt32
    D.TInt64 -> C.TInt64
    D.TVarChar _ -> C.TString
    _ -> error $ "Meta.Type.data_to_cbp: not implemented: " ++ show typ
