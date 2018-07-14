module Meta.Map (
    -- * Reexports
    Map.Map
    , Map.lookup
    , Map.fromList
    , Map.fromListWith
    , Map.toList
    -- * Custom
    , from_list
    , group_by
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
