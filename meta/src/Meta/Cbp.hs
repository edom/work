{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Meta.Cbp where

import Prelude ()
import Meta.Prelude

import qualified Meta.Cbp_internal as I
import qualified Meta.Data as D
import qualified Meta.Java as J
import qualified Meta.Type as MT

-- * Type

type Type = I.Type

-- * Class

type Class = I.Class

type Class_name = I.Class_name

c_empty :: Class
c_empty = I.c_empty

get_class_name :: Class -> Class_name
get_class_name = I.cName

set_class_name :: Class_name -> Class -> Class
set_class_name n c = c { I.cName = n }

map_class_name :: (Class_name -> Class_name) -> Class -> Class
map_class_name f c = set_class_name (f $ get_class_name c) c

-- * Generate DTO (Data Transfer Object) class from relational database table

-- | Configuration for 'genDto'.
data GenDto
    -- | Internal. Do not use. Use 'defGenDto'
    = MkGenDto {
        gdCommentField :: Bool -- ^ Whether to comment every field.
        , gdImmutable :: Bool -- ^ Whether to make every field const/final.
    } deriving (Read, Show)

-- | Default configuration for 'genDto'.
defGenDto :: GenDto
defGenDto = MkGenDto {
        gdCommentField = False
        , gdImmutable = False
    }

gen_dto :: D.Table -> Class
gen_dto = genDtoWith defGenDto

-- TODO generate getter methods
-- TODO generate setter methods
-- TODO generate 0-param constructor
-- TODO generate n-param constructor
-- TODO make every field private
genDtoWith :: GenDto -> D.Table -> Class
genDtoWith conf table = c_empty {
        I.cName = tabName
        , I.cMembers = [
            I.MLineComment $ " Generated by Meta.Cbp.genDto from table \"" ++ tabName ++ "\"."
        ] ++ concatMap mapCol cols
    }
    where
        tabName = D.t_get_name table
        cols = D.t_get_cols table
        wantCommentField = gdCommentField conf
        mapCol :: D.Col -> [I.Member]
        mapCol col =
            if wantCommentField
                then [comment, field]
                else [field]
            where
                comment = I.MLineComment $ " Column \"" ++ colName ++ "\" (" ++ show typ ++ ")."
                field = I.MField (I.MkField (MT.data_to_cbp typ) colName)
                typ = D.c_get_type col
                colName = D.c_get_name col

-- * Transform to Java

toJavaClass :: Class -> J.Class
toJavaClass cls =
    J.defClass { J.cPkg = pkg, J.cMembers = concat mem }
    |> J.c_set_name name
    where
        mem = toJavaMember <$> I.cMembers cls
        pkg = I.cPkg cls
        name = I.cName cls

toJavaMember :: I.Member -> [J.Member]
toJavaMember mem = case mem of
    I.MField fld -> [J.MField $ toJavaField fld]
    I.MLineComment x -> [J.MLineComment x]
    I.MBlockComment x -> [J.MBlockComment x]
    _ -> error $ "Meta.Cbp.toJavaMember: not implemented: " ++ show mem

toJavaField :: I.Field -> J.Field
toJavaField fld = J.mkField typ nam
    where
        typ = MT.cbp_to_java $ I.fType fld
        nam = I.fName fld
