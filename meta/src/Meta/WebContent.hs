module Meta.WebContent (
    Url
    , Html_doc(..)
    , html_empty
    , add_styles
    , Content(..)
    , Java_resource_path
    , Tag_name
    , html_elm
    , html_atr
    -- * Monoid
    , content_empty
    , content_append
    , content_concat
) where

import qualified Meta.Data as D
import qualified Meta.Html as H

type Url = String

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

content_empty :: Content
content_empty = CEmpty

content_append :: Content -> Content -> Content
content_append = CSeq

content_concat :: [Content] -> Content
content_concat = foldr content_append content_empty
