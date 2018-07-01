{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Sitemap where

import qualified Data.Time as Ti

import qualified Item as I

import Text.RawString.QQ

-- | This should contain trailing slash.
type UriPrefix = String

sitemap :: UriPrefix -> Site -> Sitemap
sitemap prefix = map (item prefix)

item :: UriPrefix -> I.Item a -> Entry
item prefix i =
    MkEntry (prefix ++ I._path i) (I._modTime i)

{- |
A string that is an XML document.
-}
type Xml_string = String

data Entry
    = MkEntry
    {
        _url :: String
        , _modTime :: Ti.UTCTime
    }

type Sitemap = [Entry]

type Site = forall a. [I.Item a]

render :: Sitemap -> Xml_string
render entries =
    unlines $ header : xml_entries ++ ["</urlset>"]
    where
        header =
            [r|<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">|]
        xml_entries = flip map entries $ \ ent ->
            let
                loc = _url ent
                lastmod = formatTime $ _modTime ent
            in
                "<url><loc>" ++ loc ++ "</loc><lastmod>" ++ lastmod ++ "</lastmod><changefreq>weekly</changefreq></url>"

-- * Time

formatTime :: Ti.UTCTime -> String
formatTime = Ti.formatTime Ti.defaultTimeLocale "%FT%H:%M:%SZ"
