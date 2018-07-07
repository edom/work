module Meta.List (
    -- * Reexports from "Data.List"
    L.intercalate
    -- * Reexports from "Data.List.Split"
    , S.splitOn
    -- * Interleavings
    , interleavings2
    -- * Indexing
    , at
    -- * Associative lists
    , kv_upsert
    , kv_get
    , kv_get_or
    -- * Updating
    , update_first
    -- * Array-like update
    , replace
) where

import qualified Data.List as L
import qualified Data.List.Split as S

{- |
Can we generalize this to @interleavings :: [[a]] -> [[a]]@?
-}
interleavings2 :: [a] -> [a] -> [[a]]
interleavings2 = go
    where
        go [] bs = [bs]
        go as [] = [as]
        go (a:as) (b:bs) =
            map (a:) (go as (b:bs))
            ++
            map (b:) (go (a:as) bs)

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at _ i | i < 0 = Nothing
at (x : _) 0 = Just x
at (_ : y) i = at y (i - 1)

{- |
@kv_upsert k v x@ replaces the first occurrence of @(k,_)@ in @x@ with @(k,v)@,
or inserts @(k,v)@ if there is no such occurrence.
-}
kv_upsert :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
kv_upsert k v [] = [(k, v)]
kv_upsert k v ((j, _) : r) | k == j = (k, v) : r
kv_upsert k v (_ : r) = kv_upsert k v r

{- |
@kv_get k x@ gets the first occurrence of @(k,v)@ in @x@, for any @v@.
-}
kv_get :: (Eq k) => k -> [(k, v)] -> [v]
kv_get k = map snd . filter (\ (j, _) -> k == j)

{- |
@kv_get_or def k x@ gets the first occurrence of @(k,v)@ in @x@, for any @v@.
If there are no such entries, this returns @def@.
-}
kv_get_or :: (Eq k) => v -> k -> [(k, v)] -> v
kv_get_or def k list =
    case kv_get k list of
        [] -> def
        x : _ -> x

update_first :: (a -> Bool) -> (a -> a) -> [a] -> [a]
update_first cond change = loop
    where
        loop [] = []
        loop (x : y) | cond x = change x : y
        loop (x : y) = x : loop y

replace :: a -> Int -> a -> [a] -> [a]
replace padding index_ x list = loop index_ list
    where
        loop index [] | index <= 0 = [x]
        loop index (_ : z) | index <= 0 = x : z
        loop index [] = padding : loop (index - 1) []
        loop index (y : z) = y : loop (index - 1) z
