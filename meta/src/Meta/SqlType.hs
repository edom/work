module Meta.SqlType where

import qualified Data.Char as C

-- | Database column data type.
data Type
    = Boolean
    | Int32
    | Int64
    | VarChar Int -- ^ the limit is the maximum number of characters, not bytes
    | Numeric Int Int -- ^ precision (total digit count), scale (fraction digit count)
    deriving (Read, Show)

to_sql :: Type -> String
to_sql typ = case typ of
    Boolean -> "BOOLEAN"
    Int32 -> "INTEGER"
    Int64 -> "BIGINT"
    VarChar n -> "VARCHAR(" ++ show n ++ ")"
    Numeric precision scale -> "NUMERIC(" ++ show precision ++ "," ++ show scale ++ ")"
    _ -> error $ "Meta.SqlType.to_sql: not implemented: " ++ show typ

from_sql :: String -> Either String Type
from_sql str = case map C.toUpper str of
    "INTEGER" -> pure Int32
    "BIGINT" -> pure Int64
    "CHARACTER VARYING" -> pure $ VarChar maxBound -- FIXME
    _ -> Left $ "Meta.SqlType.from_sql: not implemented: " ++ str
