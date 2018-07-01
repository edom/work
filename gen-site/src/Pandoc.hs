module Pandoc where

import qualified Data.Set as Set

import qualified Network.URI as Uri

import qualified System.IO.Strict as IS

import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as Ps
import qualified Text.Pandoc.Walk as Pw

import qualified Hakyll as H

-- * Generalized read and write

readMarkdown :: (Monad m) => String -> m P.Pandoc
readMarkdown input =
    either (fail . show) return $ P.readMarkdown readerOptions input

readerOptions :: P.ReaderOptions
readerOptions = H.defaultHakyllReaderOptions
    {
        P.readerExtensions = Set.union
            (Set.fromList [P.Ext_tex_math_single_backslash, P.Ext_tex_math_dollars, P.Ext_latex_macros])
            (P.readerExtensions H.defaultHakyllReaderOptions)
    }

readMarkdownFile :: FilePath -> IO P.Pandoc
readMarkdownFile path =
    IS.readFile path >>= readMarkdown

-- * Demoting headers

{- |
This may be useful when you incorporate
a multi-section blog post into a template.
-}
demoteHeaders :: P.Pandoc -> P.Pandoc
demoteHeaders = Ps.headerShift 1

writeHtml :: P.Pandoc -> String
writeHtml = P.writeHtmlString writerOptions

writerOptions :: P.WriterOptions
writerOptions = H.defaultHakyllWriterOptions
    {
        P.writerHtml5 = True
        , P.writerHTMLMathMethod = P.MathJax ""
    }

-- * Pandoc

{- |
Add the prefix to all relative URLs in all links in the given document.
-}
absolutifyLinks
    :: String -- ^ the prefix; this should have trailing slash
    -> P.Pandoc
    -> P.Pandoc

absolutifyLinks prefix =
    Pw.walk g
    where
        g (P.Link attr inlines (url, title))
            | not (Uri.isURI url)
            = P.Link attr inlines (prefix ++ url, title)
        g x = x

type URL = String
