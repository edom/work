{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Item where

import qualified Data.Time as Ti

import qualified Multitask as Mul

import qualified Dictionary as D
import qualified File as F
import qualified Pandoc as HP

import qualified Text.Pandoc as TP

data Item a
    = MkItem
    {
        _path :: FilePath
        , _payload :: a
        , _modTime :: Ti.UTCTime
    }

instance D.Member String (Item TP.Pandoc) where
    member k = D.member k . _payload

markdown :: FilePath -> FilePath -> IO (Item TP.Pandoc)
markdown dir relPath =
    MkItem relPath
    <$> HP.readMarkdownFile path
    <*> F.getModificationTime path
    where
        path = dir F.</> relPath

stat :: FilePath -> FilePath -> IO (Item a)
stat dir relPath =
    MkItem relPath (error $ "should not load " ++ path)
    <$> F.getModificationTime path
    where
        path = dir F.</> relPath

-- * Internals

markdownFilesUnder :: FilePath -> IO [Item TP.Pandoc]
markdownFilesUnder dir =
    F.files_under dir
    >>= Mul.concurrent 4 . map (markdown dir) . filter (`F.extensionIs` ".md")
