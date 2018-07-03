module Meta.WebContent (
    Html_doc(..)
    , html_empty
    , add_styles
    , Content(..)
    , Tag_name
    , html_elm
    , html_atr
    -- * Constructors
    , raw
    , seq
    , text
    , Url
    , Url_relative
    , link_internal
    , Java_resource_path
    , java_resource
    -- * Monoid
    , content_empty
    , content_append
    , content_concat
) where

import Prelude ()
import Meta.Prelude

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

type Url_relative = String

link_internal :: Url -> Content -> Content
link_internal = CLink

java_resource :: Java_resource_path -> Content
java_resource = CJavaRes

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

raw :: String -> Content
raw = CRaw

seq :: [Content] -> Content
seq = content_concat

text :: String -> Content
text = CText

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
