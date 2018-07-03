{-|

* Pretty URL pattern matching isn't supported.
Set up a reverse proxy to rewrite the URL.

* See also:

    * Ur/Web

-}
module Meta.Web (
    WC.Url
    -- * Site
    , Site(..)
    , empty
    , add_page
    , add_pages
    -- * Page
    , WP.Method
    , WP.Page(..)
    , WP.Content_type
    , WP.set_content_type
    , WP.mk_page
    , WP.page_empty
    -- * Questionable abstraction
    , WC.Html_doc(..)
    , WC.html_empty
    , WC.add_styles
    -- * Content
    , WC.Content(..)
    , WC.Java_resource_path
    , WC.Tag_name
    , WC.html_elm
    , WC.html_atr
    -- * Monoid
    , WC.content_empty
    , WC.content_append
    , WC.content_concat
) where

import qualified Meta.WebContent as WC
import qualified Meta.WebPage as WP

data Site
    = MkSite {
        sPages :: [WP.Page]
    }
    deriving (Read, Show)

empty :: Site
empty = MkSite []

add_page :: WC.Url -> WC.Content -> Site -> Site
add_page url con sit = sit { sPages = sPages sit ++ [WP.page_empty { WP.pUrl = url, WP.pContent = con }] }

add_pages :: [WP.Page] -> Site -> Site
add_pages pages site = site { sPages = sPages site ++ pages }
