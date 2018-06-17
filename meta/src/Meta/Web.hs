{-|

* Pretty URL pattern matching isn't supported.
Set up a reverse proxy to rewrite the URL.

* See also:

    * Ur/Web

-}
module Meta.Web where

import qualified Meta.Data as D

type Url = String

data Site
    = MkSite {
        sPages :: [Page]
    }
    deriving (Show, Read)

empty :: Site
empty = MkSite []

add_page :: Url -> Content -> Site -> Site
add_page url con sit = sit { sPages = sPages sit ++ [MkPage url con] }

data Page
    = MkPage {
        pUrl :: Url -- ^ static, no pattern matching
        , pContent :: Content
    }
    deriving (Show, Read)

data Content
    = CEmpty
    | CRaw String -- ^ pass-through unmodified
    | CText String -- ^ escaped
    | CSeq [Content] -- ^ sequence/concatenation/juxtaposition
    | CLink Url Content -- ^ CLink url caption: hyperlink
    | CGetParam String -- ^ value of a GET parameter
    | CPostParam String -- ^ value of a POST parameter
    | CView D.Query -- ^ generated view for query
    deriving (Show, Read)
