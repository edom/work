module List
where

-- * Indexing

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at _ i | i < 0 = Nothing
at (x : _) 0 = Just x
at (_ : y) i = at y (i - 1)

-- * Associative lists

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

-- * Updating

update_first :: (a -> Bool) -> (a -> a) -> [a] -> [a]
update_first cond change = loop
    where
        loop [] = []
        loop (x : y) | cond x = change x : y
        loop (x : y) = x : loop y

-- * Array-like update

replace :: a -> Int -> a -> [a] -> [a]
replace padding index_ x list = loop index_ list
    where
        loop index [] | index <= 0 = [x]
        loop index (_ : z) | index <= 0 = x : z
        loop index [] = padding : loop (index - 1) []
        loop index (y : z) = y : loop (index - 1) z
