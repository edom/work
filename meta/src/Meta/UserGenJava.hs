module Meta.UserGenJava (
    generate_java
    , get_pom_xml_dir
    , maven_recompile
    , generate_sql_ddl
) where

import Prelude ()
import Meta.Prelude

import qualified Data.List as L

import qualified Meta.Cbp as C
import qualified Meta.Data as D
import qualified Meta.Data_internal as DI
import qualified Meta.File as F
import qualified Meta.Java as J
import qualified Meta.JavaRender as JR
import qualified Meta.JavaWebApp as JWA
import qualified Meta.Maven as M
import qualified Meta.MavenCmd as MavenCmd
import qualified Meta.MavenDep as MD
import qualified Meta.SqlType as SqlType
import qualified Meta.WrapM as V
import qualified Meta.Xml as X

{- |
Generate a Java web application.

What is generated:

* \<output\_prefix\>

    * \<artifact\_id\>/

        * src\/main\/java\/: Java source files

        * sql\/create.sql: SQL DDL statements

        * Dockerfile: Docker recipe

            * We assume that Docker is used to distribute the application, not to build the application.
            The application is built on a development machine or a build machine.
-}
generate_java :: JWA.App -> IO ()
generate_java app_without_runtime =
    let
        app = add_runtime_dependency app_without_runtime |> sort_dependencies
        files = F.prepend_dir (get_pom_xml_dir app) <$> ([pom_xml_file] ++ java_files ++ sql_files ++ docker_files)
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

        docker_files = [
                F.text "Dockerfile" $
                    "FROM openjdk:8-jre-slim\n"
                    ++ "ADD target /root/target\n"
            ]
    in
        mapM_ F.file_write_verbose files
    where
        sort_dependencies :: JWA.App -> JWA.App
        sort_dependencies app = JWA.set_dependencies (L.sort $ JWA.get_dependencies app) app

add_runtime_dependency :: JWA.App -> JWA.App
add_runtime_dependency = JWA.add_dependencies (dep_runtime : deps_third_party)
    where
        runtime_version = "0.0.0"
        jetty_version = "9.4.11.v20180605"
        dep_runtime = MD.compile "com.spacetimecat" "meta-rt-java" runtime_version
        deps_third_party = [
                MD.compile "javax.inject" "javax.inject" "1"
                , MD.compile "javax.servlet" "javax.servlet-api" "3.1.0"
                , MD.compile "ch.qos.logback" "logback-classic" "1.2.3" -- logging framework implementing SLF4J
                , MD.compile "com.google.inject" "guice" "4.0" -- dependency injection framework
                , MD.compile "com.zaxxer" "HikariCP" "2.7.8" -- JDBC connection pool
                , MD.compile "org.eclipse.jetty" "jetty-server" jetty_version -- HTTP server
                , MD.compile "org.eclipse.jetty" "jetty-servlet" jetty_version -- Jetty implementation of Servlet API
                , MD.compile "org.postgresql" "postgresql" "42.2.2" -- PostgreSQL JDBC driver
            ]

-- | Path to the directory containing the pom.xml.
get_pom_xml_dir :: JWA.App -> FilePath
get_pom_xml_dir app = JWA._output_prefix app F.</> JWA.get_artifact_id app

maven_recompile :: JWA.App -> IO ()
maven_recompile app = MavenCmd.recompile (get_pom_xml_dir app)

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
