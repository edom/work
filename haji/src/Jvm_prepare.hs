{- |
This module deals with replacing references to constant pool entries.
-}
module Jvm_prepare
where

import Data.Int
    (
        Int8
        , Int16
        , Int32
        , Int64
    )
import Data.Word
    (
        Word8
        , Word16
        , Word32
    )

import qualified Control.Monad as M
import qualified Data.List as L

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Unsafe as Bsu

import qualified Data.ByteString.UTF8 as Bu

import qualified Jvm_io as Z

import Jvm_arch -- XXX

-- * Dereferencing constant pool entries

{- |
This transforms a binary-representation 'Z.Class' into a prepared 'Class'
by dereferencing the constant pool.

Those two @Class@es differ.
-}
resolve_class :: Z.Class -> Either String Class
resolve_class c = do
    Mk_class
    <$> name
    <*> super
    <*> fields
    <*> methods
    <*> pool
    <*> pure []
    <*> pure False
    where

        name =
            Z.cp_get_class c (Z.c_this c)
            >>= Z.cp_get_utf8 c

        super =
            case Z.c_super c of
                0 -> Right Nothing
                s -> Just <$> (Z.cp_get_class c s >>= Z.cp_get_utf8 c)

        fields =
            M.forM (Z.c_fields c) $ \ f -> do
                name <- Z.cp_get_utf8 c (Z.fi_name f)
                type_ <- Z.cp_get_utf8 c (Z.fi_descriptor f) >>= Z.parse_field_type
                attrs <- mapM resolve_attribute (Z.fi_attributes f) -- XXX not yet used
                return $ Mk_field (Z.fi_access f) name type_

        methods =
            M.forM (Z.c_methods c) $ \ m -> do
                name <- Z.cp_get_utf8 c (Z.mi_name m)
                type_ <- Z.cp_get_utf8 c (Z.mi_descriptor m) >>= Z.parse_method_type
                attrs <- mapM resolve_attribute (Z.mi_attributes m)
                let
                    contents = [ c | Mk_attribute name c <- attrs
                        , name == Bu.fromString "Code" ]
                    mb_content = case contents of
                        x : _ -> Just x
                        _ -> Nothing
                body <- case mb_content of
                    Nothing -> Right Missing
                    Just content -> Bytecode <$> Z.parse_code_attr_content content
                return $ Mk_method (Z.mi_access m) name type_ body

        resolve_attribute :: Z.Attribute -> Either String Attribute
        resolve_attribute a =
            Mk_attribute
                <$> Z.cp_get_utf8 c (Z.a_name a)
                <*> pure (Z.a_content a)

        pool :: Either String [Constant]
        pool =
            mapM resolve_entry (Z.c_pool c)

        resolve_entry e = case e of
            Z.P_utf8 a -> Right $ C_string a
            Z.P_integer a -> Right $ C_integer a
            Z.P_float a -> Right $ C_float a
            Z.P_long a -> Right $ C_long a
            Z.P_double a -> Right $ C_double a
            Z.P_class i -> C_class <$> Z.cp_get_utf8 c i
            Z.P_string i -> C_string <$> Z.cp_get_utf8 c i
            Z.P_fieldref i_c i_nt -> do
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_fname, i_ftype) <- Z.cp_get_nameandtype c i_nt
                fname <- Z.cp_get_utf8 c i_fname
                ftype <- Z.cp_get_utf8 c i_ftype >>= Z.parse_field_type
                return $ C_fieldref cname fname ftype
            Z.P_methodref i_c i_nt -> do
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_mname, i_mtype) <- Z.cp_get_nameandtype c i_nt
                mname <- Z.cp_get_utf8 c i_mname
                mtype <- Z.cp_get_utf8 c i_mtype >>= Z.parse_method_type
                return $ C_methodref cname mname mtype
            Z.P_interfacemethodref i_c i_nt -> do -- FIXME this is duplicate
                cname <- Z.cp_get_class c i_c >>= Z.cp_get_utf8 c
                (i_mname, i_mtype) <- Z.cp_get_nameandtype c i_nt
                mname <- Z.cp_get_utf8 c i_mname
                mtype <- Z.cp_get_utf8 c i_mtype >>= Z.parse_method_type
                return $ C_methodref cname mname mtype
            Z.P_nameandtype _ _ -> Right C_resolved
            Z.P_unused -> Right C_resolved
