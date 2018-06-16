module Meta.Relat where

import qualified Data.List as L

import qualified Meta.RdbCol as C

-- * Table

-- | Database table name.
type DbTabName = String

data Table
    -- | Internal. Do not use. Use 'defTable'.
    = MkTable {
        tName :: DbTabName
        , tCols :: [C.Col]
        , tConstraints :: [Constraint]
    } deriving (Read, Show)

data Constraint
    = KPrimaryKey [C.Col]
    | KForeignKey [C.Col] Table [C.Col]
    deriving (Read, Show)

mkTable :: DbTabName -> [C.Col] -> Table
mkTable nam cols = MkTable nam cols []

defTable :: Table
defTable = MkTable "" [] []

addPrimaryKey :: [C.Col] -> Table -> Table
addPrimaryKey cols tab = tab { tConstraints = tConstraints tab ++ [KPrimaryKey cols] }

-- * Query

{- |
This is similar to Linq.
-}
data Query
    = QFrom Table -- ^ select all rows of the table
    | QProject [C.Col] Query -- ^ select some columns of subquery
    | QJoin Query Query
    | QFilter Exp Query -- ^ select only satisfying rows from subquery; the expression must be a boolean expression
    | QSkip Integer Query -- ^ skip first rows of subquery
    | QLimit Integer Query -- ^ take only first rows of subquery
    deriving (Read, Show)

-- | Perhaps we should introduce an intermediate module Meta.Sql?
renderSqlSelect :: Query -> String
renderSqlSelect q = case q of
    QFrom t ->
        "SELECT * FROM " ++ tName t
    QProject cols subq ->
        "SELECT " ++ L.intercalate "," (map C.getName cols)
        ++ " FROM (" ++ renderSqlSelect subq ++ ") tmp"
    QFilter cond subq ->
        "SELECT * FROM (" ++ renderSqlSelect subq ++ ") tmp WHERE " ++ renderSqlExp cond
    _ ->
        error $ "renderSqlSelect: not implemented: " ++ show q
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

-- * Expression

data Exp
    = ECol C.Col
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

-- * Reexports from "Meta.RdbCol"

type Col = C.Col

colInt64 = C.colInt64
colVarChar = C.colVarChar
