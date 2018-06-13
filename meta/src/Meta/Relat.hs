module Meta.Relat where

import qualified Data.List as L

-- * Type

type Name = String

data Type
    = TInt32
    | TInt64
    | TVarChar Int -- ^ the limit is the maximum number of characters, not bytes
    | TNullable Type
    deriving (Read, Show)

-- * Column

data Column
    -- | Internal. Do not use. Use column constructors.
    = MkColumn {
        cType :: Type
        , cName :: Name
    } deriving (Read, Show)

-- * Column constructors

colInt64 :: Name -> Column
colInt64 name = MkColumn TInt64 name

colVarChar :: Int -> Name -> Column
colVarChar limit name = MkColumn (TVarChar limit) name

-- * Table

data Table
    -- | Internal. Do not use. Use 'defTable'.
    = MkTable {
        tName :: Name
        , tCols :: [Column]
        , tConstraints :: [Constraint]
    } deriving (Read, Show)

data Constraint
    = KPrimaryKey [Column]
    | KForeignKey [Column] Table [Column]
    deriving (Read, Show)

mkTable :: Name -> [Column] -> Table
mkTable nam cols = MkTable nam cols []

defTable :: Table
defTable = MkTable "" [] []

addPrimaryKey :: [Column] -> Table -> Table
addPrimaryKey cols tab = tab { tConstraints = tConstraints tab ++ [KPrimaryKey cols] }

-- * Query

data Query
    = QFrom Table
    | QProject [Column] Query
    | QJoin Query Query
    | QSelect Exp Query -- ^ the expression must be a boolean expression
    deriving (Read, Show)

-- | Perhaps we should introduce an intermediate module Meta.Sql?
renderSqlSelect :: Query -> String
renderSqlSelect q = case q of
    QFrom t ->
        "SELECT * FROM " ++ tName t
    QProject cols subq ->
        "SELECT " ++ L.intercalate "," (map cName cols)
        ++ " FROM (" ++ renderSqlSelect subq ++ ") tmp"
    QSelect cond subq ->
        "SELECT * FROM (" ++ renderSqlSelect subq ++ ") tmp WHERE " ++ renderSqlExp cond
    _ ->
        error $ "renderSqlSelect: not implemented: " ++ show q
    where
        renderSqlExp :: Exp -> String
        renderSqlExp e = case e of
            ECol col -> cName col
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
    = ECol Column
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
