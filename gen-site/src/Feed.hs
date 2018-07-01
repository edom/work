{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Feed
where

import Control.Arrow ((>>>))

import qualified Data.Map.Strict as Mps

import qualified Data.Time as Ti

import qualified Text.Pandoc as P

import qualified Dictionary as D
import qualified File as F
import qualified Metadata as Md
import qualified Pandoc as HP

data Post
    = MkPost
    {
        _pandoc :: P.Pandoc
        , _feedNames :: [Name]
        , _relPath :: FilePath
        , _modTime :: Ti.UTCTime
    }

instance D.Member String Post where
    member k = D.member k . _pandoc

data Feed
    = MkFeed
    {
        _name :: Name
        , _posts :: [Post]
    }

type Name = String

collect :: [Post] -> [Feed]
collect =
    concatMap (\ p -> map (\ f -> (f, p)) $ _feedNames p)
    >>> groupByKey
    >>> map (uncurry MkFeed)

readPost :: FilePath -> FilePath -> IO Post
readPost postRoot relPath = do
    let path = postRoot F.</> relPath
    pandoc <- HP.readMarkdownFile path
    let
        feeds =
            if Md.isPublished pandoc
                then maybe [] Md.stringList $ D.lookup "feed" pandoc
                else []
    MkPost pandoc feeds relPath <$> F.getModificationTime path

{- |
Transform a relation into a map.

Collect the values having the same key.

@
groupByKey [(0,a), (0,b), (1,a), (2,b)] = [(0, [a,b]), (1,[a]), (2,[b])]
@
-}
groupByKey :: (Ord k) => [(k, v)] -> [(k, [v])]
groupByKey = Mps.toList . Mps.fromListWith (++) . map (\ (k, v) -> (k, [v]))
