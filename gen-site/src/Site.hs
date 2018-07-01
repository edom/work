{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Site generation.

Compile blog posts (usually in Markdown format) into a static website.
-}
module Site
where

import qualified Control.Monad as M
import qualified Data.Either as Ei
import qualified Data.List as L
import qualified Data.Monoid as Mo
import qualified System.IO as Si
import qualified System.IO.Error as IE
import qualified System.Exit as X

import qualified GHC.Conc as C

import qualified System.IO.Strict as Is

import qualified Control.Monad.Trans.State as S

import qualified System.Process as Pr

import qualified Data.Time as Ti

import qualified Text.Pandoc as P

import qualified Error as Er
import qualified Mutex as Mx
import qualified Multitask as Mul

import qualified Dictionary as Di
import qualified Dependency as D
import qualified Feed as Fd
import qualified File as F
import qualified Metadata as Md
import qualified Pandoc as HP
import qualified Route as HR
import qualified Sitemap as Sm
import qualified Template as T

-- * Compiling posts into a static website

data Post
    = MkPost Ti.UTCTime FilePath P.Pandoc

{- |
Compile posts into a static website.
-}
compile :: PostConfig -> IO ()
compile pc = do
    let
        sc = _siteConf pc
        base_uri = _prefix sc
        post_root = abs_post_root pc
        input_root = _inputRoot sc
        output_root = input_root F.</> _outputRoot sc
        post_template_path = input_root F.</> _templatePath pc
        common_dictionary = _commonDic pc
        -- TODO move out route :: (MonadRoute m IO) => m () into Config
        route :: S.State HR.Env ()
        route = do
            unlessM (Md.hasMetadata "published") $ HR.moveInto "draft"
            HR.replaceExtension ".html"
        compute_relative_output_path post_ =
            HR._re_path $ S.execState route $ HR.MkEnv (Fd._relPath post_) $ Fd._pandoc post_
    putstrln <- mk_putstrln
    post_template <- do
        putstrln $ "loading " ++ post_template_path
        T.readTemplateFile post_template_path
    putstrln $ "configuration: " ++ show sc
    inputs <- do
        filter (`F.extensionIs` ".md") <$> F.files_under post_root
    putstrln $ "inputs: " ++ unwords inputs
    posts <- fmap Ei.rights $ Mul.concurrent C.numCapabilities $ flip map inputs $ \ relative_path ->
        Er.try $ Er.prepend ("error loading " ++ relative_path ++ ": ") $ do
            putstrln $ "loading " ++ relative_path
            Fd.readPost post_root relative_path `IE.catchIOError` Er.throw
    -- Generate sitemap.xml.
    -- Only generate entries for published posts.
    -- Drafts are not in sitemap.
    let
        sitemap_path = output_root F.</> "sitemap.xml"
        sitemap_entries = flip concatMap posts $ \ post ->
            let
                default_url_path = "/" ++ F.dropExtension (compute_relative_output_path post)
                url = base_uri ++ maybe default_url_path Md.stringify (Di.lookup "url-path" $ Fd._pandoc post)
                mod_time = Fd._modTime post
            in
                if Md.isPublished post
                    then [Sm.MkEntry url mod_time]
                    else []
        sitemap_xml = Sm.render sitemap_entries
    putstrln $ "writing " ++ sitemap_path
    F.write_file sitemap_path sitemap_xml
    -- generate html files
    M.forM_ posts $ \ post -> do
        let
            relative_input_path = Fd._relPath post
            input_path = post_root F.</> relative_input_path
            relative_output_path = compute_relative_output_path post
            output_path = output_root F.</> relative_output_path
            line = relative_output_path ++ ": "
        -- TODO footerTemplatePath?
        -- TODO dependencies :: Template -> [FilePath]
        D.dependsOn output_path [input_path, post_template_path] >>= \case
            D.Keep ->
                putstrln $ line ++ "up-to-date"
            D.Update reason -> do
                putstrln $ line ++ "rebuild because " ++ pretty reason
                let pan2 = HP.absolutifyLinks (base_uri ++ "/") $ HP.demoteHeaders $ Fd._pandoc post
                    post_dictionary =
                        T.metadata (Md.fromPandoc pan2)
                        Mo.<> T.literal "path" (F.dropExtension relative_input_path)
                        Mo.<> T.literal "prefix" base_uri
                        Mo.<> T.literal "last-modified" (Sm.formatTime $ Fd._modTime post)
                        Mo.<> T.literal "body-class" "narrow"
                        Mo.<> T.literal "body" (HP.writeHtml pan2)
                        Mo.<> T.function "concat" (return . concat)
                        Mo.<> common_dictionary
                T.apply post_template_path post_dictionary post_template
                    >>= F.write_file output_path
    -- Copy all files in 'raw' to '_site'.
    -- FIXME use 'cp -a' instead to preserve the modtime
    raw_rel_paths <- F.files_under $ input_root F.</> "raw"
    M.forM_ raw_rel_paths $ \ rel_path -> do
        let
            src = input_root F.</> "raw" F.</> rel_path
            dst = output_root F.</> rel_path
        putstrln $ "copying " ++ rel_path
        F.copy_file src dst
    where
        pretty D.OutputNotExist = "output does not exist"
        pretty (D.InputChanged changedInputs) = "[" ++ L.intercalate ", " changedInputs ++ "] changed"
-- * Generating CSS with Clay

{- |
Process the given file path with Clay.

The input file is a Haskell source code.

Returns the CSS.
-}
clay :: FilePath -> IO String
clay inputPath = do
    style_hs <- Is.readFile inputPath
    (Just hi, Just ho, _, hp) <- Pr.createProcess (Pr.shell "stack runghc")
        {
            Pr.std_in = Pr.CreatePipe
            , Pr.std_out = Pr.CreatePipe
        }
    Si.hPutStr hi style_hs
    exitCode <- Pr.waitForProcess hp
    css <- Is.hGetContents ho
    case exitCode of
        X.ExitSuccess -> return css
        X.ExitFailure n ->
            fail $ "style compilation of " ++ inputPath ++ " failed with exit code " ++ show n

-- * Generating feeds

data Entry
    = MkEntry
    {
        modTime :: Ti.UTCTime
        , xml :: String
    }
    deriving (Show, Read)

generateFeeds :: FeedConfig -> IO ()
generateFeeds fc = do
    let
        pc = _postConf fc
        sc =  _siteConf pc
        postRoot = abs_post_root pc
        base_uri = _prefix sc
    putstrln <- mk_putstrln
    posts <-
        F.files_under postRoot
            >>= Mul.concurrent 4 . map (Fd.readPost postRoot) . filter (`F.extensionIs` ".md")
    M.forM_ (Fd.collect $ filter Md.isPublished posts) $ \ feed -> do
        let
            atomItemTemplatePath = "template/atom-entry.xml"
            atomTemplatePath = "template/atom.xml"
            name = Fd._name feed
        atomTemplate <- do
            putstrln name
            T.readTemplateFile atomTemplatePath
        entries <- M.forM (Fd._posts feed) $ \ post -> do
            let relPath = Fd._relPath post
                updated = Fd._modTime post
                dic =
                    T.metadata (Md.fromPandoc $ Fd._pandoc post)
                    <> T.literal "root" base_uri
                    <> T.literal "url" (F.dropExtension relPath)
                    <> T.literal "updated" (Sm.formatTime updated)
            putstrln relPath
            MkEntry updated <$> T.applyFile atomItemTemplatePath dic
        let
            dic =
                T.literal "title" (_feedTitle fc)
                <> T.literal "root" base_uri
                <> T.literal "url" "/"
                <> T.literal "feed-id" (_mkFeedId fc name)
                <> T.literal "author-name" (_authorName fc)
                <> T.literal "author-email" (_authorEmail fc)
                <> T.literal "updated" (Sm.formatTime $ maximum $ map modTime entries)
                <> T.literal "body" (concatMap xml entries)
        T.apply atomTemplatePath dic atomTemplate
            >>= F.write_file (_feedDir fc F.</> (name ++ ".xml"))
    where
        (<>) = (Mo.<>)
        infixr 6 <>

-- | Common configuration.
data Config
    = MkConfig
    {
        _numThreads :: Int
        , _prefix :: BaseUri -- ^ base URI; example: @"http://www.example.com"@; do not use trailing space
        , _inputRoot :: FilePath -- ^ empty means current directory
        , _outputRoot :: FilePath -- ^ can be absolute, or relative to '_inputRoot'
    }
    deriving (Show, Read)

-- | Do not use trailing space.
type BaseUri = String

data PostConfig
    = MkPostConfig
    {
        _postRoot :: FilePath -- ^ can be absolute, or relative to '_inputRoot'
        , _templatePath :: FilePath -- ^ can be absolute, or relative to '_inputRoot'
        , _siteConf :: Config
        , _commonDic :: T.Dic
    }

def :: Config
def = MkConfig C.numCapabilities "http://www.example.com" "" "_site"

mkConfig
    :: BaseUri
    -> FilePath -- ^ empty means current working directory
    -> Config

mkConfig baseUri inputRoot = MkConfig C.numCapabilities baseUri inputRoot "_site"

defPostConfig :: PostConfig
defPostConfig = MkPostConfig "post" "template/post.html" def Di.empty

abs_post_root :: PostConfig -> FilePath
abs_post_root pc = _inputRoot (_siteConf pc) F.</> (_postRoot pc)

data FeedConfig
    = MkFeedConfig
    {
        _feedDir :: FilePath -- ^ can be absolute, or relative to '_outputRoot'
        , _feedTitle :: String
        , _authorName :: String
        , _authorEmail :: String
        , _postConf :: PostConfig
        , _mkFeedId :: String -> String
    }

defFeedConfig :: FeedConfig
defFeedConfig = MkFeedConfig "feed" "" "" "" defPostConfig id

-- * Working with Monads

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mc ma = do
    c <- mc
    M.when c ma

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mc ma = do
    c <- mc
    M.unless c ma

-- * Internal

mk_putstrln :: IO (String -> IO ())
mk_putstrln = do
    mutex <- Mx.new
    return $ Mx.with mutex . putStrLn
