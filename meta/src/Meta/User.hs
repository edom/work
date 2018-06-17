{- |
Everything is experimental.

Everything may change without notice.
-}
module Meta.User where

import qualified Data.List as L

import qualified Meta.Cbp as C
import qualified Meta.Data as D
import qualified Meta.Data_internal as DI
import qualified Meta.File as F
import qualified Meta.Hs as H
import qualified Meta.HsMod as HM
import qualified Meta.HsRecord as HR
import qualified Meta.HsType as HT
import qualified Meta.Java as J
import qualified Meta.JavaType as JT
import qualified Meta.JavaRender as JR
import qualified Meta.JavaWebApp as JWA
import qualified Meta.Maven as M
import qualified Meta.Prop as P
import qualified Meta.Web as W
import qualified Meta.WrapM as V
import qualified Meta.Xml as X

-- * Support

-- | Flipped function application.
(|>) :: a -> (a -> b) -> b
(|>) = (P.|>)
infixl 0 |>

-- * File

type File = F.File

file_path :: File -> String
file_path = F.get_path

file_content :: File -> String
file_content = F.get_content

file_prepend_dir :: String -> File -> File
file_prepend_dir = F.prepend_dir

file_write_verbose :: File -> IO ()
file_write_verbose file = do
    putStrLn $ "Writing " ++ file_path file
    F.write file
    
-- * Haskell

-- | Generate Data Transfer Object module file.
hs_gen_dtos :: Hs_mod_name -> [D.Table] -> File
hs_gen_dtos mod_name tables =
    H.renderModuleFile $ H.mkModule mod_name $ concatMap H.genDto tables

hs_render_module_file :: Hs_module -> File
hs_render_module_file = H.renderModuleFile

-- ** Record

type Hs_record = HR.Record

hs_rec_generate_module :: Hs_record -> Hs_module
hs_rec_generate_module = HR.generateModule

hs_rec_mk :: Hs_mod_name -> Hs_typ_name -> [Hs_rec_field] -> Hs_record
hs_rec_mk = HR.mkRecord

hs_rec_set_ders :: [Hs_der] -> Hs_record -> Hs_record
hs_rec_set_ders = HR.setDers

type Hs_rec_field = HR.Field

type Hs_var_name = HT.VarName

hs_rec_mk_field :: Hs_var_name -> Hs_type -> Hs_rec_field 
hs_rec_mk_field = HR.mkField

hs_getter_camel :: Hs_rec_field -> Hs_rec_field
hs_getter_camel = HR.getterCamel

hs_setter_camel :: Hs_rec_field -> Hs_rec_field
hs_setter_camel = HR.setterCamel

-- ** Type

type Hs_type = HT.Type

hs_Int32 :: Hs_type
hs_Int32 = HT.int32

hs_Int64 :: Hs_type
hs_Int64 = HT.int64

hs_String :: Hs_type
hs_String = HT.string

type Hs_mod_name = H.Mod_name

type Hs_typ_name = HT.TypName

type Hs_module = HM.Module

-- ** Class names

type Hs_der = HT.SymCls

sc_Read ::HT.SymCls
sc_Read = HT.sc_Read

sc_Show ::HT.SymCls
sc_Show = HT.sc_Show

-- * Java web application

type Jwa_app = JWA.App

jwa_empty :: Jwa_app
jwa_empty = JWA.empty

{- |
Generate a Java web application.

What is generated:

* \<output\_prefix\>

    * \<artifact\_id\>/

        * src\/main\/java\/: Java source files

        * sql\/create.sql: SQL DDL statements
-}
jwa_render :: Jwa_app -> IO ()
jwa_render app = do
    mapM_ file_write_verbose files
    where
        project = JWA._project app

        files = F.prepend_dir (output_prefix ++ artifact_id) <$> ([pom_xml_file] ++ java_files ++ sql_files)

        pom_xml_file = F.text "pom.xml" $ X.render_doc $ M.to_pom_xml project

        java_files = F.prepend_dir "src/main/java" . JR.render_class_file <$> java_classes
        cbp_dto_classes = C.map_class_name (JWA._dto_class_name_prefix app ++) . C.gen_dto <$> tables
        tables = JWA.aTables app
        java_dto_classes = C.toJavaClass <$> cbp_dto_classes
        java_servlet_class = JWA.get_java_servlet_class app
        java_classes = J.set_pkg (JWA._package app) <$> (java_dto_classes ++ [java_servlet_class])

        artifact_id = jwa_get_artifact_id app
        output_prefix = JWA._output_prefix app

        sql_files = F.prepend_dir "sql" <$> [F.text "create.sql" sql_ddl]
        sql_ddl = generate_sql_ddl tables

type Output_prefix = FilePath

jwa_set_output_prefix :: Output_prefix -> Jwa_app -> Jwa_app
jwa_set_output_prefix path app = app { JWA._output_prefix = path }

jwa_set_package :: String -> Jwa_app -> Jwa_app
jwa_set_package pkg app = app { JWA._package = pkg }

jwa_set_servlet_class_name :: String -> Jwa_app -> Jwa_app
jwa_set_servlet_class_name nam app = app { JWA._servlet_class_name = nam }

jwa_set_gav :: Maven_group_id -> Maven_artifact_id -> Maven_version -> Jwa_app -> Jwa_app
jwa_set_gav = JWA.set_gav

jwa_add_deps :: [Maven_dep] -> Jwa_app -> Jwa_app
jwa_add_deps deps app = JWA.set_deps (JWA.get_deps app ++ deps) app

jwa_dep_provided :: Maven_group_id -> Maven_artifact_id -> Maven_dep_ver -> Maven_dep
jwa_dep_provided = JWA.dep_provided

jwa_get_artifact_id :: Jwa_app -> Maven_artifact_id
jwa_get_artifact_id = M.pArtifactId . JWA._project

jwa_add_page :: Url -> Content -> Jwa_app -> Jwa_app
jwa_add_page = JWA.add_page

-- ** Injections

type Injection = JWA.Injection

{- |
Every injection adds these to the servlet class:

* a private final field

* a constructor parameter

* an assignment statement in the constructor
-}
jwa_add_injections :: [Injection] -> Jwa_app -> Jwa_app
jwa_add_injections injs app = app { JWA._injections = JWA._injections app ++ injs }

type Inject_name = JWA.Inject_name

type Field_name = JWA.Field_name

injection :: Java_type -> Field_name -> Injection
injection = JWA.injection

inj_named :: Inject_name -> Injection -> Injection
inj_named = JWA.named

-- ** Java types

type Java_type = J.Type

jt_DataSource :: Java_type
jt_DataSource = JT.dataSource

-- ** Tables

jwa_set_tables :: [Table] -> Jwa_app -> Jwa_app
jwa_set_tables = JWA.set_tables

-- * Website description language

type Url = W.Url

type Content = W.Content

c_seq :: [Content] -> Content
c_seq = W.CSeq

c_text :: String -> Content
c_text = W.CText

c_link_internal :: Url -> Content -> Content
c_link_internal = W.CLink

-- | Generate view for query.
c_view :: D.Query -> Content
c_view = W.CView

-- * Data query language

type Query = D.Query

q_from :: Table -> Query
q_from = DI.QFrom

-- * Data description language

type Table = D.Table

type Table_name = String

type Column = D.Col

type Column_name = String

mk_table :: Table_name -> [Column] -> Table
mk_table = DI.mkTable

tab_add_primary_key :: [Column] -> Table -> Table
tab_add_primary_key = DI.addPrimaryKey

tab_set_schema :: String -> Table -> Table
tab_set_schema = D.t_set_schema

tab_set_DataSource_field_name :: Field_name -> Table -> Table
tab_set_DataSource_field_name = D.t_set_DataSource_field_name

col_set_short_title :: String -> Column -> Column
col_set_short_title = DI.setShortTitle

col_set_long_title :: String -> Column -> Column
col_set_long_title = DI.setLongTitle

col_set_title :: String -> Column -> Column
col_set_title t = col_set_long_title t . col_set_short_title t

col_varchar :: Int -> Column_name -> Column
col_varchar = DI.colVarChar

col_int32 :: Column_name -> Column
col_int32 = DI.colInt32

col_int64 :: Column_name -> Column
col_int64 = DI.colInt64

{- |
Generate SQL DDL (Data Definition Language) statements for schemas and tables.

What is generated: CREATE SCHEMA and CREATE TABLE statements.

TODO:

* escape strings
-}
generate_sql_ddl :: [D.Table] -> String
generate_sql_ddl tabs = V.render $ do
    mapM_ v_sch schemas
    if null schemas then V.nop else V.break
    mapM_ v_tab tabs
    where
        schemas = L.nub [ s | t <- tabs, Just s <- [DI._schema t] ]
        v_sch sch = V.atom ("CREATE SCHEMA " ++ sch ++ ";") >> V.break
        v_col col = do
            let cnam = D.c_get_name col
                ctyp = D.c_get_type col
                cnul = D.c_get_nullable col
                styp = DI.sql_type_name ctyp
                snul = if cnul then "NULL" else "NOT NULL"
            V.atom (cnam ++ " " ++ styp ++ " " ++ snul)
        v_con con = case con of
            DI.KPrimaryKey cols -> do
                V.atom "PRIMARY KEY (" >> V.commaSep (V.atom . D.c_get_name <$> cols) >> V.atom ")"
            _ -> error $ "Meta.User.generate_sql_ddl.v_con: not implemented: " ++ show con
        v_tab tab = do
            let m_schema = D.t_get_schema tab
                schema_dot = maybe "" (++ ".") m_schema
                table = D.t_get_name tab
                cols = D.t_get_cols tab
                cons = D.t_get_constraints tab
            V.atom ("CREATE TABLE " ++ schema_dot ++ table ++ " (") >> V.break
            V.indented $ do
                V.commaSep $ map (>> V.break) $ map v_col cols ++ map v_con cons
            V.atom ");" >> V.break

-- * Maven

type Maven_group_id = M.Group_id

type Maven_artifact_id = M.Artifact_id

type Maven_version = M.Version

type Maven_dep = M.Dep

type Maven_dep_ver = M.Dep_ver
