module Meta.WebPage (
    Method
    , Page(..)
    , Content_type
    , set_content_type
    , mk_page
    , page_empty
) where

import qualified Meta.WebContent as WC

type Method = String

data Page
    = MkPage {
        pUrl :: WC.Url -- ^ static, no pattern matching
        , pContent :: WC.Content
        , pContentType :: String
        , pMethod :: String
    }
    deriving (Read, Show)

type Content_type = String

set_content_type :: Content_type -> Page -> Page
set_content_type t p = p { pContentType = t }

mk_page :: WC.Url -> WC.Content -> Page
mk_page url content = page_empty {
        pUrl = url
        , pContent = content
    }

page_empty :: Page
page_empty = MkPage {
        pUrl = ""
        , pContent = WC.CEmpty
        , pContentType = "text/html; charset=UTF-8"
        , pMethod = "GET"
    }
