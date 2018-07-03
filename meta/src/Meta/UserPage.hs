{-# LANGUAGE RecordWildCards #-}

module Meta.UserPage (
    -- * Route language
    Page
    , get
    , post
    , W.Content_type
    , W.set_content_type
    -- * Content language
    , C.Content
    , C.raw
    , C.seq
    , C.text
    , C.Url
    , C.Url_relative
    , C.link_internal
    , C.Java_resource_path
    , C.java_resource
    -- ** CRUD
    , Crud
    , mk_crud
    , get_base_url
    , tabulate
    , form_for_insert
    , crud_pages
) where

import Prelude ()
import Meta.Prelude

import Meta.Web (
        Content
        , Page
        , Url
    )

import qualified Control.Applicative as A

import qualified Meta.Data as D
import qualified Meta.UserHtml as H
import qualified Meta.Web as W
import qualified Meta.WebContent as C

get :: Url -> C.Content -> W.Page
get url content = (W.mk_page url content) { W.pMethod = "GET" }

post :: Url -> C.Content -> W.Page
post url content = (W.mk_page url content) { W.pMethod = "POST" }

data Crud
    = MkCrud {
        _cTable :: D.Table
        , _cBaseUrl :: C.Url_relative
    } deriving (Read, Show)

mk_crud :: D.Table -> Crud
mk_crud table = MkCrud {
        _cTable = table
        , _cBaseUrl = table_base_url table
    }

get_base_url :: Crud -> C.Url_relative
get_base_url = _cBaseUrl

table_base_url :: D.Table -> C.Url_relative
table_base_url table = component m_ds_field ++ component m_schema ++ "/" ++ name
    where
        component :: Maybe String -> String
        component = maybe "" ("/" ++)
        m_ds_field = D.t_DataSource_field_name table
        m_schema = D.t_get_schema table
        name = D.t_get_name table

-- | Table view for query.
tabulate :: D.Query -> Content
tabulate = W.CView

-- | Form for inserting a row into the table.
form_for_insert :: Crud -> Content
form_for_insert MkCrud{..} = H.form $
    [
        H.atr "method" "POST"
        , H.atr "action" action
        , H.span [H.atr "class" "form_title", C.text form_title]
    ]
    ++ map input_for_column eligible_cols
    ++ [
        H.input [
            H.atr "type" "submit"
            , H.atr "value" ("Insert " ++ name)
            , H.atr "class" "form_sole_submit_button"
        ]
    ]
    where
        action = _cBaseUrl ++ "/insert"
        form_title = "Insert " ++ name
        name = D.t_get_name _cTable
        cols = D.t_get_cols _cTable
        eligible_cols = filter (not . D.get_auto_increment) cols

{- |
Generate CRUD pages.
-}
crud_pages :: Crud -> [W.Page]
crud_pages crud@MkCrud{..} = insert_page
    where
        insert_page = [
                get url_insert (form_for_insert crud)
                , post url_insert (C.text "not implemented")
            ]
        url_insert = _cBaseUrl ++ "/insert"

input_for_column :: D.Column -> Content
input_for_column col =
    H.label $
        [
            H.atr "class" "form_field"
            , H.span [H.atr "class" "title", C.text title]
            , H.input [H.atr "type" "text", H.atr "name" name, H.atr "placeholder" title]
        ]
        ++ null_checkbox
    where
        name = D.c_get_name col
        can_be_null = D.c_get_nullable col
        null_checkbox =
            if can_be_null
                then
                    [
                        H.label [
                            H.atr "class" "null_checkbox"
                            -- This breaks if a table contains a nullable column named @name@ and another column named @name_empty_means_null@.
                            , H.input [H.atr "type" "checkbox", H.atr "name" (name ++ "_empty_means_null"), H.atr "checked" "checked"]
                            , H.span [C.text "empty means null"]
                        ]
                    ]
                else []
        title = maybe name id $ D.c_long_title col A.<|> D.c_short_title col
