{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Dictionary where

import Prelude hiding (lookup)

import qualified Control.Applicative as A
import qualified Data.Monoid as Mo

import qualified Data.Map.Lazy as Map

import qualified Text.Pandoc as P

empty :: Dictionary k v
empty = MkDictionary $ const Nothing

-- * Map-like things

{- |
[@k@] key type

[@c@] container type

[@v@] value type
-}
class Lookup k c v where
    lookup :: (Monad m) => k -> c -> m v

instance (Show k) => Lookup k (Dictionary k v) v where
    lookup k d = maybe (fail $ "key not found: " ++ show k) return (_unDictionary d k)

instance (Ord k, Show k) => Lookup k (Map.Map k v) v where
    lookup k = maybe (fail $ "Hakyll.Metadata.lookup: key not found: " ++ show k) return . Map.lookup k

instance Lookup String P.Meta P.MetaValue where
    lookup k = lookup k . P.unMeta

instance Lookup String P.Pandoc P.MetaValue where
    lookup k (P.Pandoc meta _) = lookup k meta

{- |
[@k@] key type

[@c@] container type
-}
class Member k c where
    member :: k -> c -> Bool

instance (Ord k) => Member k (Map.Map k v) where
    member = Map.member

instance Member String P.Meta where
    member k = member k . P.unMeta

instance Member String P.Pandoc where
    member k (P.Pandoc meta _) = member k meta

-- * Internals

{- |
Add/override entries using the 'Mo.Monoid' instance.
-}
newtype Dictionary k v
    = MkDictionary { _unDictionary :: k -> Maybe v }

-- This allows overriding entries.
instance Mo.Monoid (Dictionary k v) where
    mempty =
        MkDictionary $ const $ fail "empty dictionary"
    mappend (MkDictionary f) (MkDictionary g) =
        MkDictionary $ \ k -> f k A.<|> g k

fromMap :: (Ord k) => Map.Map k v -> Dictionary k v
fromMap m = MkDictionary $ \ k -> Map.lookup k m
