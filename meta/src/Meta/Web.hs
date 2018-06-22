{-|

* Pretty URL pattern matching isn't supported.
Set up a reverse proxy to rewrite the URL.

* See also:

    * Ur/Web

-}
module Meta.Web where

import qualified Meta.Data as D
import qualified Meta.Html as H

type Url = String

data Site
    = MkSite {
        sPages :: [Page]
    }
    deriving (Read, Show)

empty :: Site
empty = MkSite []

add_page :: Url -> Content -> Site -> Site
add_page url con sit = sit { sPages = sPages sit ++ [page_empty { pUrl = url, pContent = con }] }

add_pages :: [Page] -> Site -> Site
add_pages pages site = site { sPages = sPages site ++ pages }

data Page
    = MkPage {
        pUrl :: Url -- ^ static, no pattern matching
        , pContent :: Content
        , pContentType :: String
    }
    deriving (Read, Show)

type Content_type = String

set_content_type :: Content_type -> Page -> Page
set_content_type t p = p { pContentType = t }

mk_page :: Url -> Content -> Page
mk_page url content = page_empty {
        pUrl = url
        , pContent = content
    }

page_empty :: Page
page_empty = MkPage {
        pUrl = ""
        , pContent = CEmpty
        , pContentType = "text/html; charset=UTF-8"
    }

data Html_doc
    = Mk_Html_doc {
        _h_title :: String
        , _h_styles :: [Url]
        , _h_body :: Content
    } deriving (Read, Show)

html_empty :: Html_doc
html_empty = Mk_Html_doc {
        _h_title = ""
        , _h_styles = []
        , _h_body = CEmpty
    }

add_styles :: [Url] -> Html_doc -> Html_doc
add_styles urls html = html { _h_styles = _h_styles html ++ urls }

type Java_resource_path = String

data Content
    = CEmpty
    | CRaw String -- ^ pass-through unmodified
    | CText String -- ^ escaped; move to Chtmla?
    | CSeq Content Content -- ^ sequence/concatenation/juxtaposition
    | CLink Url Content -- ^ CLink url caption: hyperlink
    | CGetParam String -- ^ value of a GET parameter
    | CPostParam String -- ^ value of a POST parameter
    | CView D.Query -- ^ generated view for query
    | CJavaRes Java_resource_path
    | CHtml Html_doc -- rename to CHtmlDoc? remove? c_html_doc :: Html_doc -> [Content] -> Content
    | Chtmla (H.Html Content) -- rename to CHtml?
    deriving (Read, Show)

type Tag_name = String

html_elm :: Tag_name -> [Content] -> Content
html_elm tag children = Chtmla $ H.Elm tag (H.concat $ map unwrap children)
    where
        unwrap :: Content -> H.Html Content
        unwrap (Chtmla x) = x
        unwrap x = H.Pure x

html_atr :: H.Name -> H.Value -> Content
html_atr nam val = Chtmla $ H.EAtr nam val

-- * Monoid

content_empty :: Content
content_empty = CEmpty

content_append :: Content -> Content -> Content
content_append = CSeq

content_concat :: [Content] -> Content
content_concat = foldr content_append content_empty
