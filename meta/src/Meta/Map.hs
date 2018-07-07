module Meta.Map (
    -- * Reexports
    Map.Map
    , Map.lookup
    , Map.fromList
    , Map.fromListWith
    , Map.toList
    -- * Custom
    , from_list
) where

import qualified Data.Map as Map

{- |
This is easier to use than 'Map.fromList' if the key is derived from the value.
-}
from_list :: (Ord k) => (a -> k) -> [a] -> Map.Map k a
from_list key vals = Map.fromList $ map (\ val -> (key val, val)) vals
