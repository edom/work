{-# LANGUAGE ApplicativeDo #-}

{- |

An example web application.

-}
module Meta.Example where

import Meta.User ((|>))

import qualified Meta.SqlCon as S
import qualified Meta.User as U

-- * Fresh (\"greenfield\") approaches

{- $
* Write the metaprogram in Haskell.

    * Describe your data\/model\/ontology.

    * Describe your routes.

* Run the metaprogram to generate the target program in target language.

    * Generate the web application.

        * Generate the Data Transfer Object (DTO) classes.

        * Generate the views.

* Write the business logic in the target language.

* Link generated target program with runtime and harness.
-}

main :: IO ()
main = U.generate_java app
    where
        app = U.app_empty
            |> U.set_maven_coordinates "com.spacetimecat" "meta-example-java" "0.0.0"
            |> U.set_package "com.spacetimecat.meta.example"
            |> U.set_servlet_class_name "MySiteHttpServlet"
            |> U.add_dependencies dependencies
            |> U.add_injections injections
            |> U.add_pages pages
            |> U.set_tables all_tables
        dependencies = []
        injections =
            [
                U.injection U.jt_DataSource "ds_test" |> U.set_inject_name "test"
            ]
        pages =
            [
                get "/" (
                    U.html [
                        U.link_internal "/customer" (U.text "Customers")
                        , U.link_internal "/1" (U.text "page 1")
                    ]
                )
                , get "/customer" (
                    U.html [
                        U.tabulate (U.from t_customer)
                    ]
                )
                , get "/1" (U.text "this is page 1")
                , U.get "/css/style.css" (
                    U.java_resource "css/style.css"
                ) |> U.set_content_type "text/css; charset=UTF-8"
            ]
        get url content =
            U.get url (
                U.html [content]
                |> U.add_styles ["/css/style.css"]
            )

mk_table :: U.Table_name -> [U.Column] -> U.Table
mk_table name cols =
    U.mk_table name cols
    |> U.set_schema "example"
    |> U.set_DataSource_field_name "ds_test"

all_tables :: [U.Table]
all_tables = [
        t_customer
        , t_sku
        , t_order
    ]

t_customer :: U.Table
t_customer = mk_table "customer" [
        c_id64
        , U.col_varchar 128 "name" |> U.set_title "Name"
    ]
    |> U.add_primary_key [c_id64]

t_order :: U.Table
t_order = mk_table "order" [
        c_id64
        , U.col_varchar 16 "ref_num"
            |> U.set_short_title "RefNum"
            |> U.set_long_title "Reference number"
        , U.col_int64 "customer_id"
        , U.col_int64 "sku_id"
    ]
    |> U.add_primary_key [c_id64]

t_sku :: U.Table
t_sku = mk_table "sku" [
        c_id64
        , U.col_varchar 16 "code"
        , U.col_varchar 128 "name"
    ]
    |> U.add_primary_key [c_id64]

c_id64 :: U.Column
c_id64 = U.col_int64 "id" |> U.set_title "Id"

-- * Legacy (\"brownfield\") approaches

{- $
* Generate data transfer objects (DTOs) from existing SQL database tables.
-}

import_sql_database :: IO ()
import_sql_database = S.test

-- * Scratch

-- Example: Generate Haskell DTO
main1 :: IO ()
main1 = mapM_ U.file_write_verbose files
    where
        files = map (U.file_prepend_dir "gen") [
                U.hs_gen_dtos "MyDto" all_tables
            ]

main2 :: IO ()
main2 = do
    putStrLn $ U.file_content $ U.hs_render_module_file example
    where
        example :: U.Hs_module
        example = U.hs_rec_generate_module rec
            where
                rec =
                    U.hs_rec_mk "Mod" "Rec" flds
                    |> U.hs_rec_set_ders [U.sc_Read, U.sc_Show]
                flds = [
                        U.hs_rec_mk_field "foo" U.hs_Int32 |> U.hs_getter_camel |> U.hs_setter_camel
                        , U.hs_rec_mk_field "bar" U.hs_Int64
                        , U.hs_rec_mk_field "qux" U.hs_String
                    ]
