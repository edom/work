{-# LANGUAGE ApplicativeDo #-}

{- |

An example web application.

-}
module Meta.Example where

import Meta.User ((|>))

import qualified Meta.SqlCon as S
import qualified Meta.User as U

import qualified Meta.ExampleTables as T

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
main = U.jwa_render app
    where
        app = U.jwa_empty
            |> U.jwa_set_gav "com.spacetimecat" "meta-example-java" "0.0.0"
            |> U.jwa_set_deps [
                U.jwa_dep_provided "javax.servlet" "javax.servlet-api" "3.1.0"
            ]
            |> U.jwa_add_page "/" (U.c_seq (U.c_text "hello world") (U.c_link_internal "/1" (U.c_text "page 1")))
            |> U.jwa_add_page "/1" (U.c_text "this is page 1")
            |> U.jwa_set_tables T.tables

-- * Legacy (\"brownfield\") approaches

{- $
* Generate data transfer objects (DTOs) from existing SQL database tables.
-}

main0 :: IO ()
main0 = S.test

-- * Scratch

-- Example: Generate Haskell DTO
main1 :: IO ()
main1 = mapM_ U.file_write_verbose files
    where
        files = map (U.file_prepend_path "gen/") [
                U.hs_gen_dtos "MyDto" T.tables
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
