{-# LANGUAGE TypeSynonymInstances #-}

{- |
* Warnings

    * Everything is experimental.

    * Everything may change without notice.
-}
module Meta.User (
    -- * Define table
    Table
    , Table_name
    , mk_table
    , set_schema
    -- ** Define column
    , Column
    , Column_name
    , col_varchar
    , col_int32
    , col_int64
    , col_numeric
    , col_boolean
    , auto_increment
    , nullable
    -- *** Define table caption
    , Short_title
    , Long_title
    , set_short_title
    , set_long_title
    , set_title
    , set_titles
    -- *** Define DataSource field name
    , set_DataSource_field_name
    -- ** Define table constraint
    , add_primary_key
    -- * Define application
    , App
    , app_empty
    , Output_prefix
    , set_output_prefix
    , set_tables
    -- ** Define Maven project
    , Maven_group_id
    , Maven_artifact_id
    , Maven_version
    , set_maven_coordinates
    -- ** Define Java project
    , set_package
    , set_servlet_class_name
    -- *** Define dependencies
    , Maven_dep
    , Maven_dep_ver
    , add_dependencies
    , get_dependencies
    , set_dependencies
    , compile
    , provided
    -- ** Add pages
    , add_pages
    -- ** Add injections
    , Injection
    , add_injections
    -- *** Define injections
    , Java_type
    , Field_name
    , injection
    , jt_DataSource
    , Inject_name
    , set_inject_name
    -- ** Generate application
    , generate_java
    , get_pom_xml_dir
    -- ** Generate command-line interface for controlling the application
    , command_line
    , command_line_
    -- * Route language
    , Page
    , get
    , post
    , Content_type
    , set_content_type
    -- * Content language
    , Content
    , raw
    , seq
    , text
    , Url
    , link_internal
    , tabulate
    , form_for_insert
    , Java_resource_path
    , java_resource
    -- ** HTML subset
    , Html_doc
    , C_html(..)
    -- *** HTML element
    , Tag
    , elm
    -- **** Predefined HTML elements
    , h1
    , p
    , span
    , div
    , form
    , label
    , input
    -- *** HTML attribute
    , Atr
    , atr
    -- * Query language
    , Query
    , from
    -- * Support
    , (|>)
    -- ** File
    , File
    , file_path
    , file_content
    , file_prepend_dir
    , file_write_verbose
    -- * Build application
    , maven_recompile
    -- * Experimental
    , get_artifact_id
    , generate_sql_ddl
    -- * Haskell
    , hs_gen_dtos
    , hs_render_module_file
    -- ** Record
    , Hs_record
    , hs_rec_generate_module
    , hs_rec_mk
    , hs_rec_set_ders
    , Hs_rec_field
    , Hs_var_name
    , hs_rec_mk_field
    , hs_getter_camel
    , hs_setter_camel
    -- ** Type
    , Hs_type
    , hs_Int32
    , hs_Int64
    , hs_String
    , Hs_mod_name
    , Hs_typ_name
    , Hs_module
    -- ** Class names
    , Hs_der
    , sc_Read
    , sc_Show
) where

import Prelude hiding (div, seq, span)

import qualified Control.Applicative as A
import qualified Control.Monad as Monad
import qualified Data.List as L
import qualified System.Environment as Env

import qualified System.Directory as Dir

import qualified Meta.Cbp as C
import qualified Meta.Data as D
import qualified Meta.Data_internal as DI
import qualified Meta.File as F
import qualified Meta.Hs as H
import qualified Meta.HsMod as HM
import qualified Meta.HsRecord as HR
import qualified Meta.HsType as HT
import qualified Meta.Html as Html
import qualified Meta.Java as J
import qualified Meta.JavaRender as JR
import qualified Meta.JavaType as JT
import qualified Meta.JavaWebApp as JWA
import qualified Meta.Maven as M
import qualified Meta.MavenCmd as MavenCmd
import qualified Meta.MavenDep as MD
import qualified Meta.Prop as P
import qualified Meta.SqlType as SqlType
import qualified Meta.Web as W
import qualified Meta.WrapM as V
import qualified Meta.Xml as X

type Table = D.Table

type Table_name = String

mk_table :: Table_name -> [Column] -> Table
mk_table = DI.mkTable

set_schema :: String -> Table -> Table
set_schema = D.t_set_schema

type Column = D.Col

type Column_name = String

col_varchar :: Int -> Column_name -> Column
col_varchar = DI.colVarChar

col_int32 :: Column_name -> Column
col_int32 = DI.colInt32

col_int64 :: Column_name -> Column
col_int64 = DI.colInt64

col_numeric :: Int -> Int -> Column_name -> Column
col_numeric precision scale = DI.mkCol (SqlType.Numeric precision scale)

col_boolean :: Column_name -> Column
col_boolean = DI.mkCol SqlType.Boolean

auto_increment :: Column -> Column
auto_increment c = c { DI._cAutoIncrement = True }

nullable :: Column -> Column
nullable c = c { DI._cNullable = True }

is_auto_increment :: Column -> Bool
is_auto_increment = DI._cAutoIncrement

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

set_DataSource_field_name :: Field_name -> Table -> Table
set_DataSource_field_name = D.t_set_DataSource_field_name

add_primary_key :: [Column] -> Table -> Table
add_primary_key = DI.addPrimaryKey

type App = JWA.App

app_empty :: App
app_empty = JWA.empty

type Output_prefix = FilePath

set_output_prefix :: Output_prefix -> App -> App
set_output_prefix path app = app { JWA._output_prefix = path }

set_tables :: [Table] -> App -> App
set_tables = JWA.set_tables

type Maven_group_id = M.Group_id

type Maven_artifact_id = M.Artifact_id

type Maven_version = M.Version

set_maven_coordinates :: Maven_group_id -> Maven_artifact_id -> Maven_version -> App -> App
set_maven_coordinates = JWA.set_gav

set_package :: String -> App -> App
set_package pkg app = app { JWA._package = pkg }

set_servlet_class_name :: String -> App -> App
set_servlet_class_name nam app = app { JWA._servlet_class_name = nam }

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

{- |
See 'Page' for content language.
-}
add_pages :: [Page] -> App -> App
add_pages = JWA.add_pages

type Injection = JWA.Injection

{- |
Every injection adds these to the servlet class:

* a private final field

* a constructor parameter

* an assignment statement in the constructor
-}
add_injections :: [Injection] -> App -> App
add_injections injs app = app { JWA._injections = JWA._injections app ++ injs }

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

{- |
Generate a Java web application.

What is generated:

* \<output\_prefix\>

    * \<artifact\_id\>/

        * src\/main\/java\/: Java source files

        * sql\/create.sql: SQL DDL statements
-}
generate_java :: App -> IO ()
generate_java app_without_runtime =
    let
        app = add_runtime_dependency app_without_runtime |> sort_dependencies
        files = F.prepend_dir (get_pom_xml_dir app) <$> ([pom_xml_file] ++ java_files ++ sql_files)
            where
                pom_xml_file = F.text "pom.xml" $ X.render_doc $ M.to_pom_xml project
                project = JWA._project app

        tables = JWA.aTables app

        java_files = F.prepend_dir "src/main/java" . JR.render_class_file <$> classes
            where
                cbp_dto_classes = C.map_class_name (JWA._dto_class_name_prefix app ++) . C.gen_dto <$> tables
                dto_classes = C.toJavaClass <$> cbp_dto_classes
                servlet_class = JWA.get_java_servlet_class app
                classes = J.set_pkg (JWA._package app) <$> (dto_classes ++ [servlet_class])

        sql_files = F.prepend_dir "sql" <$> [F.text "create.sql" sql_ddl]
            where
                sql_ddl = generate_sql_ddl tables
    in
        mapM_ file_write_verbose files
    where
        sort_dependencies :: App -> App
        sort_dependencies app = set_dependencies (L.sort $ get_dependencies app) app

add_runtime_dependency :: App -> App
add_runtime_dependency = add_dependencies (dep_runtime : deps_third_party)
    where
        runtime_version = "0.0.0"
        jetty_version = "9.4.11.v20180605"
        dep_runtime = compile "com.spacetimecat" "meta-rt-java" runtime_version
        deps_third_party = [
                compile "javax.inject" "javax.inject" "1"
                , compile "javax.servlet" "javax.servlet-api" "3.1.0"
                , compile "ch.qos.logback" "logback-classic" "1.2.3" -- logging framework implementing SLF4J
                , compile "com.google.inject" "guice" "4.0" -- dependency injection framework
                , compile "com.zaxxer" "HikariCP" "2.7.8" -- JDBC connection pool
                , compile "org.eclipse.jetty" "jetty-server" jetty_version -- HTTP server
                , compile "org.eclipse.jetty" "jetty-servlet" jetty_version -- Jetty implementation of Servlet API
                , compile "org.postgresql" "postgresql" "42.2.2" -- PostgreSQL JDBC driver
            ]

-- | Path to the directory containing the pom.xml.
get_pom_xml_dir :: App -> FilePath
get_pom_xml_dir app = JWA._output_prefix app F.</> get_artifact_id app

{- |
Command line for controlling the application and related common tasks.
-}
command_line :: App -> IO ()
command_line app = Env.getArgs >>= command_line_ app

-- | This is 'command_line' but you pass the command-line arguments yourself.
command_line_ :: App -> [String] -> IO ()
command_line_ app = parse Monad.>=> run
    where
        parse :: (Monad m) => [String] -> m Command
        parse args = case args of
            [] -> pure Nop
            "cd" : path : rest -> CmdSeq (Chdir path) <$> parse rest
            "generate" : rest -> CmdSeq (Generate app) <$> parse rest
            "help" : rest -> CmdSeq Help <$> parse rest
            "recompile" : rest -> CmdSeq (Recompile app) <$> parse rest
            _ -> fail $ "Invalid arguments: " ++ show args
        run :: Command -> IO ()
        run cmd = case cmd of
            Nop -> return ()
            Help -> do
                prog <- Env.getProgName
                putStr $
                    "Usage: " ++ prog ++ " COMMAND...\n"
                    ++ "A COMMAND is any of these:\n"
                    ++ "    cd DIR      change directory to DIR\n"
                    ++ "    help        show this\n"
                    ++ "    generate    generate Java source code\n"
                    ++ "    recompile   recompile generated Java source code\n"
            Chdir path -> Dir.setCurrentDirectory path
            CmdSeq a b -> run a >> run b
            Generate ap -> generate_java ap
            Recompile ap -> maven_recompile ap

data Command
    = Nop
    | CmdSeq Command Command
    | Chdir FilePath
    | Generate App
    | Help
    | Recompile App
    deriving (Read, Show)

type Page = W.Page

get :: Url -> Content -> Page
get url content = (W.mk_page url content) { W.pMethod = "GET" }

post :: Url -> Content -> Page
post url content = (W.mk_page url content) { W.pMethod = "POST" }

{- |
Some examples:

* \"application/octet-stream\"

* \"text/html; charset=UTF-8\"
-}
type Content_type = String

set_content_type :: Content_type -> Page -> Page
set_content_type = W.set_content_type

type Content = W.Content

raw :: String -> Content
raw = W.CRaw

seq :: [Content] -> Content
seq = W.content_concat

text :: String -> Content
text = W.CText

type Url = W.Url

link_internal :: Url -> Content -> Content
link_internal = W.CLink

-- | Table view for query.
tabulate :: D.Query -> Content
tabulate = W.CView

-- | Form for inserting a row into the table.
form_for_insert :: D.Table -> Content
form_for_insert table = form $
    [
        atr "method" "POST"
        , atr "action" action
        , span [atr "class" "form_title", text form_title]
    ]
    ++ map input_for_column eligible_cols
    ++ [input [atr "type" "submit", atr "value" ("Insert " ++ name), atr "class" "form_sole_submit_button"]]
    where
        action = component m_ds_field ++ component m_schema ++ "/" ++ name ++ "/insert"
        form_title = "Insert " ++ name
        component :: Maybe String -> String
        component = maybe "" ("/" ++)
        m_ds_field = D.t_DataSource_field_name table
        m_schema = D.t_get_schema table
        name = D.t_get_name table
        cols = D.t_get_cols table
        eligible_cols = filter (not . is_auto_increment) cols

input_for_column :: Column -> Content
input_for_column col =
    label $
        [
            atr "class" "form_field"
            , span [atr "class" "title", text title]
            , input [atr "type" "text", atr "name" name, atr "placeholder" title]
        ]
        ++ null_checkbox
    where
        name = D.c_get_name col
        can_be_null = D.c_get_nullable col
        null_checkbox =
            if can_be_null
                then
                    [
                        label [
                            atr "class" "null_checkbox"
                            -- This breaks if a table contains a nullable column named @name@ and another column named @name_empty_means_null@.
                            , input [atr "type" "checkbox", atr "name" (name ++ "_empty_means_null"), atr "checked" "checked"]
                            , span [text "empty means null"]
                        ]
                    ]
                else []
        title = maybe name id $ D.c_long_title col A.<|> D.c_short_title col

type Java_resource_path = W.Java_resource_path

java_resource :: Java_resource_path -> Content
java_resource = W.CJavaRes

type Tag = Html.Name

-- | @elm tag children@ represents HTML element with tag name @tag@ and children @children@.
elm :: Tag -> [Content] -> Content
elm = W.html_elm

h1 :: [Content] -> Content
h1 = elm "h1"

p :: [Content] -> Content
p = elm "p"

span :: [Content] -> Content
span = elm "span"

div :: [Content] -> Content
div = elm "div"

form :: [Content] -> Content
form = elm "form"

label :: [Content] -> Content
label = elm "label"

input :: [Content] -> Content
input = elm "input"

type Atr = Html.Atr

atr :: Html.Name -> Html.Value -> Content
atr = W.html_atr

type Html_doc = W.Html_doc

class C_html a where

    html :: [Content] -> a

    add_styles :: [Url] -> a -> a

instance C_html Html_doc where

    html body = W.html_empty { W._h_body = W.content_concat body }

    add_styles = W.add_styles

instance C_html Content where

    html body = W.CHtml $ html body

    add_styles urls (W.CHtml h) = W.CHtml (add_styles urls h)
    add_styles _ c = c

type Query = D.Query

from :: Table -> Query
from = DI.QFrom

-- | Flipped function application.
(|>) :: a -> (a -> b) -> b
(|>) = (P.|>)
infixl 0 |>

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

maven_recompile :: App -> IO ()
maven_recompile app = MavenCmd.recompile (get_pom_xml_dir app)

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
                styp = SqlType.to_sql ctyp
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

-- | Generate Data Transfer Object module file.
hs_gen_dtos :: Hs_mod_name -> [D.Table] -> File
hs_gen_dtos mod_name tables =
    H.renderModuleFile $ H.mkModule mod_name $ concatMap H.genDto tables

hs_render_module_file :: Hs_module -> File
hs_render_module_file = H.renderModuleFile

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

type Hs_der = HT.SymCls

sc_Read ::HT.SymCls
sc_Read = HT.sc_Read

sc_Show ::HT.SymCls
sc_Show = HT.sc_Show
