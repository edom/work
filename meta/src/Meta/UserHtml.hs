{-# LANGUAGE TypeSynonymInstances #-}

-- | HTML subset.
module Meta.UserHtml (
    -- * HTML subset
    Html_doc
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
) where

import Prelude ()
import Meta.UserPrelude

import Meta.Web (
        Url
        , Content
        , Html_doc
    )

import qualified Meta.Html as Html
import qualified Meta.Web as W

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
