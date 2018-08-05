module Meta.Map (
    -- * Reexports
    Map.Map
    , Map.empty
    , Map.lookup
    , Map.fromList
    , Map.fromListWith
    , Map.toList
    , Map.filter
    , Map.insert
    , Map.insertWith
    , Map.insertWith'
    , Map.alter
    , Map.map
    -- * Custom
    , from_list
    , group_by
    , filterM
) where

import qualified Data.Map as Map

{- |
This is easier to use than 'Map.fromList' if the key is derived from the value.
-}
from_list :: (Ord k) => (a -> k) -> [a] -> Map.Map k a
from_list key vals = Map.fromList pairs where
        pairs = map pair vals
        pair val = (key val, val)

{- |
This is similar to @groupBy@ in "Data.List".
-}
group_by :: (Ord k) => (a -> k) -> [a] -> Map.Map k [a]
group_by key vals = Map.fromListWith (++) pairs where
        pairs = map pair vals
        pair val = (key val, [val])

filterM :: (Monad m) => (v -> m Bool) -> Map.Map k v -> m (Map.Map k v)
filterM pred_ trav = do
    filtered <- mapM predM trav
    return $ Map.mapMaybe id filtered
    where
        predM e = ifM (pred_ e) (return $ Just e) (return Nothing)

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM mc mt mf = do
    c <- mc
    if c then mt else mf
