module Dynasty.Multiplay.Parlor
(
    Parlor(..)
    , getTable
    , modifyTable
)
where

import Prelude hiding (id)

import qualified Dynasty.Multiplay.Table as T

data Parlor
    = MkParlor
    {
        tables :: [T.Table]
    }

empty :: Parlor
empty = MkParlor
    []

getTable :: T.Id -> Parlor -> [T.Table]
getTable id_ = filter (\ g -> T.id g == id_) . tables

modifyTable :: T.Id -> (T.Table -> T.Table) -> Parlor -> Parlor
modifyTable id_ f s = s { tables = map (\ g -> if T.id g == id_ then f g else g) $ tables s }
