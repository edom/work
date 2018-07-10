{- |
* This module helps you generate a Java web application.

* This module should be the only module you need to import from your metaprogram.

* Warnings

    * Everything is experimental.

    * Everything may change without notice.

* Do not import modules imported by this module.
-}
module Meta.User (
    -- * Define table
    DI.Table
    , DI.Table_name
    , DI.mk_table
    , DI.set_schema
    -- ** Define column
    , DC.Column
    , DC.Column_name
    , DC.col_varchar
    , DC.col_int32
    , DC.col_int64
    , DC.col_numeric
    , DC.col_boolean
    , auto_increment
    , nullable
    -- *** Define table caption
    , DC.Short_title
    , DC.Long_title
    , DC.set_short_title
    , DC.set_long_title
    , DC.set_title
    , DC.set_titles
    -- *** Define DataSource field name
    , set_DataSource_field_name
    -- ** Define table constraint
    , DI.add_primary_key
    -- * Define application
    , JWA.App
    , JWA.app_empty
    , JWA.Output_prefix
    , JWA.set_output_prefix
    , JWA.set_tables
    -- ** Define Maven project
    , M.Maven_group_id
    , M.Maven_artifact_id
    , M.Maven_version
    , JWA.set_maven_coordinates
    -- ** Define Java project
    , JWA.set_package
    , JWA.set_servlet_class_name
    -- *** Define dependencies
    , M.Maven_dep
    , M.Maven_dep_ver
    , JWA.add_dependencies
    , JWA.get_dependencies
    , JWA.set_dependencies
    , MD.compile
    , MD.provided
    -- ** Add pages
    , JWA.add_pages
    -- ** Add injections
    , JWA.Injection
    , JWA.add_injections
    -- *** Define injections
    , JWA.Java_type
    , JWA.Field_name
    , JWA.injection
    , JWA.jt_DataSource
    , JWA.Inject_name
    , JWA.set_inject_name
    -- ** Define Docker parameters
    , JWA.get_docker_repository
    , JWA.set_docker_repository
    -- ** Define Kubernetes parameters
    , JWA.set_kube_service_name
    -- ** Generate command-line interface for controlling the application
    , Cmd.command_line
    , Cmd.command_line_
    -- * Page language
    -- ** Route language
    , Page.Page
    , Page.get
    , Page.post
    , Page.Content_type
    , Page.set_content_type
    -- ** Content language
    , Page.Content
    , Page.raw
    , Page.seq
    , Page.text
    , Page.Url
    , Page.Url_relative
    , Page.link_internal
    , Page.Java_resource_path
    , Page.java_resource
    -- ** CRUD
    , Page.Crud
    , Page.mk_crud
    , Page.get_base_url
    , Page.tabulate
    , Page.form_for_insert
    , Page.crud_pages
    -- *** Query language
    , DQ.Query
    , DQ.from
    -- ** HTML subset
    , module Meta.UserHtml
    -- * Support
    , (|>)
    -- ** File
    , F.File
    , F.file_path
    , F.file_content
    , F.file_prepend_dir
    , F.file_write_verbose
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

import Prelude ()
import Meta.Prelude

import Meta.UserHtml

import qualified Meta.Data as D
import qualified Meta.DataColumn as DC
import qualified Meta.DataQuery as DQ
import qualified Meta.Data_internal as DI
import qualified Meta.File as F
import qualified Meta.Hs as H
import qualified Meta.HsMod as HM
import qualified Meta.HsRecord as HR
import qualified Meta.HsType as HT
import qualified Meta.JavaWebApp as JWA
import qualified Meta.Maven as M
import qualified Meta.MavenDep as MD
import qualified Meta.UserCmd as Cmd
import qualified Meta.UserPage as Page

auto_increment :: D.Column -> D.Column
auto_increment = D.set_auto_increment True

nullable :: D.Column -> D.Column
nullable = D.set_nullable True

set_DataSource_field_name :: JWA.Field_name -> D.Table -> D.Table
set_DataSource_field_name = D.t_set_DataSource_field_name

-- | Generate Data Transfer Object module file.
hs_gen_dtos :: Hs_mod_name -> [D.Table] -> F.File
hs_gen_dtos mod_name tables =
    H.renderModuleFile $ H.mkModule mod_name $ concatMap H.genDto tables

hs_render_module_file :: Hs_module -> F.File
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
