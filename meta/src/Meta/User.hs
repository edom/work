{-# LANGUAGE TypeSynonymInstances #-}

{- |
* Warnings

    * Everything is experimental.

    * Everything may change without notice.
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
import qualified Meta.MavenDep as MD
import qualified Meta.Prop as P
import qualified Meta.Web as W
import qualified Meta.WrapM as V
import qualified Meta.Xml as X

-- * Define table

type Table = D.Table

type Table_name = String

mk_table :: Table_name -> [Column] -> Table
mk_table = DI.mkTable

set_schema :: String -> Table -> Table
set_schema = D.t_set_schema

-- ** Define column

type Column = D.Col

type Column_name = String

col_varchar :: Int -> Column_name -> Column
col_varchar = DI.colVarChar

col_int32 :: Column_name -> Column
col_int32 = DI.colInt32

col_int64 :: Column_name -> Column
col_int64 = DI.colInt64

col_numeric :: Int -> Int -> Column_name -> Column
col_numeric precision scale = DI.mkCol (DI.TNumeric precision scale)

col_boolean :: Column_name -> Column
col_boolean = DI.mkCol DI.TBoolean

-- *** Define table caption

type Short_title = String

type Long_title = String

set_short_title :: Short_title -> Column -> Column
set_short_title = DI.setShortTitle

set_long_title :: Long_title -> Column -> Column
set_long_title = DI.setLongTitle

set_title :: String -> Column -> Column
set_title t = set_long_title t . set_short_title t

set_titles :: Short_title -> Long_title -> Column -> Column
set_titles st lt = set_long_title lt . set_short_title st

-- *** Define DataSource field name

set_DataSource_field_name :: Field_name -> Table -> Table
set_DataSource_field_name = D.t_set_DataSource_field_name

-- ** Define table constraint

add_primary_key :: [Column] -> Table -> Table
add_primary_key = DI.addPrimaryKey

-- * Define application

type App = JWA.App

app_empty :: App
app_empty = JWA.empty

type Output_prefix = FilePath

set_output_prefix :: Output_prefix -> App -> App
set_output_prefix path app = app { JWA._output_prefix = path }

set_tables :: [Table] -> App -> App
set_tables = JWA.set_tables

-- ** Define Maven project

type Maven_group_id = M.Group_id

type Maven_artifact_id = M.Artifact_id

type Maven_version = M.Version

set_maven_coordinates :: Maven_group_id -> Maven_artifact_id -> Maven_version -> App -> App
set_maven_coordinates = JWA.set_gav

-- ** Define Java project

set_package :: String -> App -> App
set_package pkg app = app { JWA._package = pkg }

set_servlet_class_name :: String -> App -> App
set_servlet_class_name nam app = app { JWA._servlet_class_name = nam }

-- *** Define dependencies

type Maven_dep = M.Dep

type Maven_dep_ver = M.Dep_ver

add_dependencies :: [Maven_dep] -> App -> App
add_dependencies deps app = set_dependencies (get_dependencies app ++ deps) app

get_dependencies :: App -> [Maven_dep]
get_dependencies = JWA.get_deps

set_dependencies :: [Maven_dep] -> App -> App
set_dependencies = JWA.set_deps

compile :: Maven_group_id -> Maven_artifact_id -> Maven_dep_ver -> Maven_dep
compile = MD.compile

provided :: Maven_group_id -> Maven_artifact_id -> Maven_dep_ver -> Maven_dep
provided = JWA.dep_provided

-- ** Add pages

add_pages :: [Page] -> App -> App
add_pages = JWA.add_pages

-- *** Define pages

type Page = W.Page

mk_page :: Url -> Content -> Page
mk_page = W.mk_page

{- |
Some examples:

* \"application/octet-stream\"

* \"text/html; charset=UTF-8\"
-}
type Content_type = String

set_content_type :: Content_type -> Page -> Page
set_content_type = W.set_content_type

-- ** Add injections

type Injection = JWA.Injection

{- |
Every injection adds these to the servlet class:

* a private final field

* a constructor parameter

* an assignment statement in the constructor
-}
add_injections :: [Injection] -> App -> App
add_injections injs app = app { JWA._injections = JWA._injections app ++ injs }

-- *** Define injections

type Java_type = J.Type

type Field_name = JWA.Field_name

injection :: Java_type -> Field_name -> Injection
injection = JWA.injection

jt_DataSource :: Java_type
jt_DataSource = JT.dataSource

type Inject_name = JWA.Inject_name

-- | For javax.inject.Named annotation.
set_inject_name :: Inject_name -> Injection -> Injection
set_inject_name = JWA.named

-- ** Generate the application

{- |
Generate a Java web application.

What is generated:

* \<output\_prefix\>

    * \<artifact\_id\>/

        * src\/main\/java\/: Java source files

        * sql\/create.sql: SQL DDL statements
-}
generate_java :: App -> IO ()
generate_java app_without_runtime = do
    mapM_ file_write_verbose files
    where
        app :: App
        app = app_without_runtime
            |> add_dependencies (dep_runtime : deps_third_party)
            |> (\ app_ -> set_dependencies (L.sort $ get_dependencies app_) app_)

        dep_runtime = compile "com.spacetimecat" "meta-rt-java" runtime_version
            where
                runtime_version = "0.0.0"

        deps_third_party = [
                compile "javax.inject" "javax.inject" "1"
                , compile "javax.servlet" "javax.servlet-api" "3.1.0"

                -- Java logging framework implementing SLF4J
                , compile "ch.qos.logback" "logback-classic" "1.2.3"

                -- Java dependency injection framework
                , compile "com.google.inject" "guice" "4.0"

                -- JDBC connection pool
                , compile "com.zaxxer" "HikariCP" "2.7.8"

                -- Java HTTP server
                , compile "org.eclipse.jetty" "jetty-server" jetty_version

                -- Jetty implementation of Java Servlet API
                , compile "org.eclipse.jetty" "jetty-servlet" jetty_version

                -- PostgreSQL JDBC driver
                , compile "org.postgresql" "postgresql" "42.2.2"
            ]
            where
                jetty_version = "9.4.11.v20180605"

        project = JWA._project app

        files = F.prepend_dir (output_prefix ++ artifact_id) <$> ([pom_xml_file] ++ java_files ++ sql_files)

        pom_xml_file = F.text "pom.xml" $ X.render_doc $ M.to_pom_xml project

        java_files = F.prepend_dir "src/main/java" . JR.render_class_file <$> java_classes
        cbp_dto_classes = C.map_class_name (JWA._dto_class_name_prefix app ++) . C.gen_dto <$> tables
        tables = JWA.aTables app
        java_dto_classes = C.toJavaClass <$> cbp_dto_classes
        java_servlet_class = JWA.get_java_servlet_class app
        java_classes = J.set_pkg (JWA._package app) <$> (java_dto_classes ++ [java_servlet_class])

        artifact_id = get_artifact_id app
        output_prefix = JWA._output_prefix app

        sql_files = F.prepend_dir "sql" <$> [F.text "create.sql" sql_ddl]
        sql_ddl = generate_sql_ddl tables

-- * Content language

type Content = W.Content

raw :: String -> Content
raw = W.CRaw

seq :: [Content] -> Content
seq = mconcat

text :: String -> Content
text = W.CText

type Url = W.Url

link_internal :: Url -> Content -> Content
link_internal = W.CLink

-- | Generate view for query.
tabulate :: D.Query -> Content
tabulate = W.CView

type Java_resource_path = W.Java_resource_path

java_resource :: Java_resource_path -> Content
java_resource = W.CJavaRes

-- ** HTML subset

type Html_doc = W.Html_doc

class C_html a where

    html :: [Content] -> a

    add_styles :: [Url] -> a -> a

instance C_html Html_doc where

    html body = W.html_empty { W._h_body = mconcat body }

    add_styles = W.add_styles

instance C_html Content where

    html body = W.CHtml $ html body

    add_styles urls (W.CHtml h) = W.CHtml (add_styles urls h)
    add_styles _ c = c

-- * Query language

type Query = D.Query

from :: Table -> Query
from = DI.QFrom

-- * Support

-- | Flipped function application.
(|>) :: a -> (a -> b) -> b
(|>) = (P.|>)
infixl 0 |>

-- ** File

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

-- * Internal

get_artifact_id :: App -> Maven_artifact_id
get_artifact_id = M.pArtifactId . JWA._project

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

-- * Haskell

-- | Generate Data Transfer Object module file.
hs_gen_dtos :: Hs_mod_name -> [D.Table] -> File
hs_gen_dtos mod_name tables =
    H.renderModuleFile $ H.mkModule mod_name $ concatMap H.genDto tables

hs_render_module_file :: Hs_module -> File
hs_render_module_file = H.renderModuleFile

-- *** Record

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
