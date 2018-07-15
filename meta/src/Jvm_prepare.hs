{- |
This module deals with replacing references to constant pool entries.
-}
module Jvm_prepare
where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as M

import qualified Data.ByteString.UTF8 as Bu

import Meta.JvmArch -- XXX
import qualified Meta.JvmAccess as A
import qualified Meta.JvmCls as Z
import qualified Meta.JvmClsConst as K
import qualified Meta.JvmSer as S

-- * Dereferencing constant pool entries

{- |
This transforms a binary-representation 'Z.Class' into a prepared 'Class'
by dereferencing the constant pool.

Those two @Class@es differ.
-}
resolve_class :: Z.Class -> EitherString Class
resolve_class c = do
    Mk_class
    <$> class_name
    <*> super
    <*> fields
    <*> methods
    <*> pool
    <*> pure []
    <*> pure False
    where

        class_name =
            Z.cp_get_class c (Z.c_this c)
            >>= Z.cp_get_utf8 c

        super =
            case Z.c_super c of
                0 -> return Nothing
                s -> Just <$> (Z.cp_get_class c s >>= Z.cp_get_utf8 c)

        fields =
            M.forM (Z.c_fields c) $ \ f -> do
                name <- Z.cp_get_utf8 c (K.fi_name f)
                type_ <- Z.cp_get_utf8 c (K.fi_descriptor f) >>= S.parse_field_type
                attrs <- mapM resolve_attribute (K.fi_attributes f) -- XXX not yet used
                return $ Mk_field (A.get_access f) name type_

        methods =
            M.forM (Z.c_methods c) $ \ m -> do
                method_name <- Z.cp_get_utf8 c (K.mi_name m)
                type_ <- Z.cp_get_utf8 c (K.mi_descriptor m) >>= S.parse_method_type
                attrs <- mapM resolve_attribute (K.mi_attributes m)
                let
                    contents = [ content | Mk_attribute name content <- attrs
                        , name == Bu.fromString "Code" ]
                    mb_content = case contents of
                        x : _ -> Just x
                        _ -> Nothing
                body <- case mb_content of
                    Nothing -> return Missing
                    Just content -> Bytecode <$> S.parse_code_attr_content content
                return $ Mk_method (A.get_access m) method_name type_ body

        resolve_attribute :: K.Attribute -> EitherString Attribute
        resolve_attribute a =
            Mk_attribute
                <$> Z.cp_get_utf8 c (A.get_name a)
                <*> pure (A.get_content a)

        pool :: EitherString [Constant]
        pool =
            mapM resolve_entry (Z.c_pool c)

        resolve_entry e = case e of
            K.P_utf8 a -> return $ C_string a
            K.P_integer a -> return $ C_integer a
            K.P_float a -> return $ C_float a
            K.P_long a -> return $ C_long a
            K.P_double a -> return $ C_double a
            K.P_class i -> C_class <$> Z.cp_get_utf8 c i
            K.P_string i -> C_string <$> Z.cp_get_utf8 c i
            K.P_fieldref i_c i_nt -> do
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_fname, i_ftype) <- Z.cp_get_nameandtype c i_nt
                fname <- Z.cp_get_utf8 c i_fname
                ftype <- Z.cp_get_utf8 c i_ftype >>= S.parse_field_type
                return $ C_fieldref cname fname ftype
            K.P_methodref i_c i_nt -> do
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_mname, i_mtype) <- Z.cp_get_nameandtype c i_nt
                mname <- Z.cp_get_utf8 c i_mname
                mtype <- Z.cp_get_utf8 c i_mtype >>= S.parse_method_type
                return $ C_methodref cname mname mtype
            K.P_interfacemethodref i_c i_nt -> do -- FIXME this is duplicate
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_mname, i_mtype) <- Z.cp_get_nameandtype c i_nt
                mname <- Z.cp_get_utf8 c i_mname
                mtype <- Z.cp_get_utf8 c i_mtype >>= S.parse_method_type
                return $ C_methodref cname mname mtype
            K.P_nameandtype _ _ -> return C_resolved
            K.P_unused -> return C_resolved
