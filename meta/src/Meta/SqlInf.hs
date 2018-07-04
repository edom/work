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
process infsch_rows =
    map mk_table $ Map.toList $ Map.fromListWith (++) (map to_pair $ filter (\ row -> _rtSchema row `notElem` ignoredSchemas) infsch_rows)
    where
        to_pair row@MkRow{..} = ((_rtCatalog, _rtSchema, _rtTabName), [row])
        mk_table ((cat,sch,tab),rows) = MkTable cat sch tab rows

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
