{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{- |
SQL information schema.
-}
module Meta.SqlInf where

import qualified Data.Int as I

import qualified Data.Map.Strict as Map

data Row = MkRow {
        _rtCatalog :: String
        , _rtSchema :: String
        , _rtTabName :: String
        , _rtColName :: String
        , _rtNullable :: String
        , _rcDataType :: String
        , _rcCharMaxLen :: Maybe I.Int32
        , _rcNumPrec :: Maybe I.Int32
        , _rcNumScale :: Maybe I.Int32
    } deriving (Read, Show)

data Table = MkTable {
        _tCatalog :: String
        , _tSchema :: String
        , _tName :: String
        , _tCols :: [Row]
    } deriving (Read, Show)

process :: [Row] -> [Table]
process = group_by key collect
    where
        key MkRow{..} = (_rtCatalog, _rtSchema, _rtTabName)
        collect (cat,sch,tab) rows = MkTable cat sch tab rows

{- |
We could relax the constraint to @'Eq' k@
and remove the @containers@ dependency
if we use "Data.List" @sortBy@ and @groupBy@,
at the cost of speed.
-}
group_by :: (Ord k) => (a -> k) -> (k -> [a] -> b) -> [a] -> [b]
group_by key collect things = map (uncurry collect) entry_list
    where
        to_entry x = (key x, [x])
        pairs = map to_entry things
        the_map = Map.fromListWith (++) pairs
        entry_list = Map.toList the_map

class FromField f a where
    field :: f a

parse
    :: (
        Applicative f
        , FromField f String
        , FromField f (Maybe String)
        , FromField f (Maybe I.Int32)
    )
    => f Row
parse = MkRow
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> field

-- HDBC doesn't support arrays
query :: String
query = "SELECT t.table_catalog, t.table_schema, t.table_name, c.column_name, c.is_nullable, c.data_type, c.character_maximum_length, c.numeric_precision, c.numeric_scale"
    ++ " FROM information_schema.tables t JOIN information_schema.columns c"
    ++ " ON (t.table_catalog, t.table_schema, t.table_name) = (c.table_catalog, c.table_schema, c.table_name)"

ignoredSchemas :: [String]
ignoredSchemas = [
        "information_schema"
        , "pg_catalog"
    ]

readNul :: String -> Either String Bool
readNul str = case str of
    "NO" -> pure False
    "YES" -> pure True
    _ -> Left $ "Meta.SqlInf.readNul: not implemented: " ++ str
