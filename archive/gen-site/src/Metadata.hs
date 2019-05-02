{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Metadata
where

import qualified Data.List as L

import qualified Data.Map.Lazy as Map

import qualified Text.Pandoc as P

import qualified Dictionary as D
import qualified Pandoc as Pan

type Metadata = Map.Map String Value

data Value
    = String String
    | Bool Bool
    | List [Value]
    deriving (Show, Read)

instance D.Lookup String P.Meta Value where
    lookup k = fmap fromPandocMetaValue . D.lookup k

instance D.Lookup String P.Pandoc Value where
    lookup k = fmap fromPandocMetaValue . D.lookup k

stringify :: Value -> String
stringify (String x) = x
stringify (Bool x) = show x
stringify (List x) = "[" ++ L.intercalate "," (map stringify x) ++ "]"

stringList :: Value -> [String]
stringList (String x) = [x]
stringList (Bool x) = [show x]
stringList (List x) = concatMap stringList x

fromPandoc :: P.Pandoc -> Metadata
fromPandoc (P.Pandoc (P.Meta meta) _) = fmap fromPandocMetaValue meta

fromPandocMetaValue :: P.MetaValue -> Value
fromPandocMetaValue = \case
    P.MetaString y -> String y
    P.MetaInlines y -> String $ write [P.Plain y]
    P.MetaBool y -> Bool y
    P.MetaList y -> List $ map fromPandocMetaValue y
    -- FIXME
    y -> String $ show y
    where
        write :: [P.Block] -> String
        write blocks = Pan.writeHtmlString P.def $ P.Pandoc P.nullMeta blocks

-- * Standard metadata

isPublished :: (D.Member String c) => c -> Bool
isPublished = D.member "published"

class MonadMetadata m where
    hasMetadata :: String -> m Bool
