module Pandoc where

import qualified Data.Text as Text

import qualified Network.URI as Uri

import qualified System.IO.Strict as IS

import qualified Text.Pandoc as P
import qualified Text.Pandoc.Shared as Ps
import qualified Text.Pandoc.Walk as Pw

import qualified Hakyll as H

-- * Generalized read and write

readMarkdown :: (P.PandocMonad m) => String -> m P.Pandoc
readMarkdown input =
    P.readMarkdown readerOptions (Text.pack input)

readerOptions :: P.ReaderOptions
readerOptions = H.defaultHakyllReaderOptions
    {
        P.readerExtensions =
            P.extensionsFromList [P.Ext_tex_math_single_backslash, P.Ext_tex_math_dollars, P.Ext_latex_macros]
            `mappend` P.readerExtensions H.defaultHakyllReaderOptions
    }

readMarkdownFile :: FilePath -> IO P.Pandoc
readMarkdownFile path =
    IS.readFile path >>= P.runIOorExplode . readMarkdown

-- * Demoting headers

{- |
This may be useful when you incorporate
a multi-section blog post into a template.
-}
demoteHeaders :: P.Pandoc -> P.Pandoc
demoteHeaders = Ps.headerShift 1

writeHtml :: P.Pandoc -> String
writeHtml = writeHtmlString writerOptions

-- FIXME migrate from pandoc 1 to pandoc 2.
writeHtmlString :: P.WriterOptions -> P.Pandoc -> String
writeHtmlString opts doc = Text.unpack $ unsafeIgnoreError $ P.writeHtml5String opts doc

unsafeIgnoreError :: P.PandocPure a -> a
unsafeIgnoreError m = case P.runPure m of
    Left e -> error $ show e
    Right x -> x
-- end fixme

writerOptions :: P.WriterOptions
writerOptions = H.defaultHakyllWriterOptions
    {
        P.writerHTMLMathMethod = P.MathJax ""
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
