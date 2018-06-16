module Meta.User where

import qualified Meta.File as F
import qualified Meta.Hs as H
import qualified Meta.HsMod as HM
import qualified Meta.HsRecord as HR
import qualified Meta.HsType as HT
import qualified Meta.IntCbp as C
import qualified Meta.Java as J
import qualified Meta.JavaRender as JR
import qualified Meta.JavaWebApp as JWA
import qualified Meta.Maven as M
import qualified Meta.Prop as P
import qualified Meta.Relat as R
import qualified Meta.RdbCol as RC
import qualified Meta.Web as W
import qualified Meta.Xml as X

-- * Support

-- | Flipped function application.
(|>) :: a -> (a -> b) -> b
(|>) = (P.|>)
infixl 0 |>

-- * File

type File = F.File

file_content :: File -> String
file_content = F.fContent

file_prepend_path :: String -> File -> File
file_prepend_path = F.prepend_path

file_write_verbose :: File -> IO ()
file_write_verbose file = do
    putStrLn $ "Writing " ++ F.fPath file
    F.write file
    
-- * Haskell

-- | Generate Data Transfer Object module file.
hs_gen_dtos :: Hs_mod_name -> [R.Table] -> File
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

jwa_render :: Jwa_app -> IO ()
jwa_render app = do
    mapM_ file_write_verbose $ map move files
    where
        project = JWA._project app
        files = [pom_xml_file] ++ maven_java_files
        pom_xml_file = F.text "pom.xml" $ X.render_doc $ M.to_pom_xml project
        maven_java_files = F.prepend_path "src/main/java/" <$> java_files
        java_files = JR.render_class_file <$> java_classes
        cbp_dto_classes = C.map_class_name ("Row_" ++) . C.gen_dto <$> JWA.aTables app
        java_dto_classes = C.toJavaClass <$> cbp_dto_classes
        java_servlet_class = W.toJavaHttpServletClass "MySiteHttpServlet" $ JWA._site app
        java_classes = J.set_pkg "com.spacetimecat" <$> (java_dto_classes ++ [java_servlet_class])
        move file = file { F.fPath = "dist/example/" ++ F.fPath file }

jwa_set_gav :: Maven_group_id -> Maven_artifact_id -> Maven_version -> Jwa_app -> Jwa_app
jwa_set_gav = JWA.set_gav

jwa_set_deps :: [Maven_dep] -> Jwa_app -> Jwa_app
jwa_set_deps = JWA.set_deps

jwa_dep_provided :: Maven_group_id -> Maven_artifact_id -> Maven_dep_ver -> Maven_dep
jwa_dep_provided = JWA.dep_provided

-- ** Tables

jwa_set_tables :: [Table] -> Jwa_app -> Jwa_app
jwa_set_tables = JWA.set_tables

-- ** Site

type Jwa_page_url = JWA.Page_url

jwa_add_page :: Jwa_page_url -> Jwa_page_content -> Jwa_app -> Jwa_app
jwa_add_page = JWA.add_page

type Jwa_page_content = JWA.Page_content

c_seq :: Jwa_page_content -> Jwa_page_content -> Jwa_page_content
c_seq = W.CSeq

c_text :: String -> Jwa_page_content
c_text = W.CText

c_link_internal :: Jwa_page_url -> Jwa_page_content -> Jwa_page_content
c_link_internal = W.CLink

-- * Data description language

type Table = R.Table

type Table_name = String

type Column = RC.Col

type Column_name = String

mk_table :: Table_name -> [Column] -> Table
mk_table = R.mkTable

tab_add_primary_key :: [Column] -> Table -> Table
tab_add_primary_key = R.addPrimaryKey

col_set_short_title :: String -> Column -> Column
col_set_short_title = RC.setShortTitle

col_set_long_title :: String -> Column -> Column
col_set_long_title = RC.setLongTitle

col_set_title :: String -> Column -> Column
col_set_title t = col_set_long_title t . col_set_short_title t

col_varchar :: Int -> Column_name -> Column
col_varchar = RC.colVarChar

col_int32 :: Column_name -> Column
col_int32 = RC.colInt32

col_int64 :: Column_name -> Column
col_int64 = RC.colInt64

-- * Maven

type Maven_group_id = M.Group_id

type Maven_artifact_id = M.Artifact_id

type Maven_version = M.Version

type Maven_dep = M.Dep

type Maven_dep_ver = M.Dep_ver
