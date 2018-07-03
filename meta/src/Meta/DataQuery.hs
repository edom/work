module Meta.DataQuery (
    Query(..)
    , from
    , renderSqlSelect
    , Exp(..)
) where

import qualified Data.List as L

import qualified Meta.Data_internal as I
import qualified Meta.DataColumn as C

{- |
This is similar to Linq.
-}
data Query
    = From I.Table -- ^ select all rows of the table
    | Project [C.Column] Query -- ^ select some columns of subquery
    | QJoin Query Query
    | Filter Exp Query -- ^ select only satisfying rows from subquery; the expression must be a boolean expression
    | Skip Integer Query -- ^ skip first rows of subquery
    | Limit Integer Query -- ^ take only first rows of subquery
    deriving (Read, Show)

from :: I.Table -> Query
from = From

-- | Perhaps we should introduce an intermediate module Meta.Sql?
renderSqlSelect :: Query -> String
renderSqlSelect q = case q of
    From t ->
        -- TODO escape
        "SELECT " ++ L.intercalate ", " (map C.getName $ I.tCols t) ++ " FROM " ++ maybe "" (++ ".") (I._schema t) ++ I.tName t
    Project cols subq ->
        "SELECT " ++ L.intercalate ", " (map C.getName cols)
        ++ " FROM (" ++ renderSqlSelect subq ++ ") tmp"
    Filter cond subq ->
        "SELECT * FROM (" ++ renderSqlSelect subq ++ ") tmp WHERE " ++ renderSqlExp cond
    _ ->
        error $ "Meta.Data_internal.renderSqlSelect: not implemented: " ++ show q
    where
        renderSqlExp :: Exp -> String
        renderSqlExp e = case e of
            ECol col -> C.getName col
            EEq a b -> "(" ++ renderSqlExp a ++ " = " ++ renderSqlExp b ++ ")"
            ELt a b -> "(" ++ renderSqlExp a ++ " < " ++ renderSqlExp b ++ ")"
            EGt a b -> "(" ++ renderSqlExp a ++ " > " ++ renderSqlExp b ++ ")"
            ELteq a b -> "(" ++ renderSqlExp a ++ " <= " ++ renderSqlExp b ++ ")"
            EGteq a b -> "(" ++ renderSqlExp a ++ " >= " ++ renderSqlExp b ++ ")"
            ENeg a -> "(-" ++ renderSqlExp a ++ ")"
            EPlus a b -> "(" ++ renderSqlExp a ++ " + " ++ renderSqlExp b ++ ")"
            ENot a -> "(NOT " ++ renderSqlExp a ++ ")"
            EAnd a b -> "(" ++ renderSqlExp a ++ " AND " ++ renderSqlExp b ++ ")"
            EOr a b -> "(" ++ renderSqlExp a ++ " OR " ++ renderSqlExp b ++ ")"

data Exp
    = ECol C.Column
    | EEq Exp Exp
    | ELt Exp Exp
    | EGt Exp Exp
    | ELteq Exp Exp
    | EGteq Exp Exp
    | ENeg Exp
    | EPlus Exp Exp
    | ENot Exp
    | EAnd Exp Exp
    | EOr Exp Exp
    deriving (Read, Show)
