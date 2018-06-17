module Meta.Data_internal where

import qualified Data.List as L

data Table
    -- | Internal. Do not use. Use 'defTable'.
    = MkTable {
        tName :: DbTabName
        , tCols :: [Col]
        , tConstraints :: [Constraint]
        , _schema :: Maybe String
        , _t_ds_field :: Maybe String
    } deriving (Read, Show)

data Constraint
    = KPrimaryKey [Col]
    | KForeignKey [Col] Table [Col]
    deriving (Read, Show)

-- | Database table name.
type DbTabName = String

mkTable :: DbTabName -> [Col] -> Table
mkTable nam cols = defTable {
        tName = nam
        , tCols = cols
    }

defTable :: Table
defTable = MkTable {
        tName = ""
        , tCols = []
        , tConstraints = []
        , _schema = Nothing
        , _t_ds_field = Nothing
    }

addPrimaryKey :: [Col] -> Table -> Table
addPrimaryKey cols tab = tab { tConstraints = tConstraints tab ++ [KPrimaryKey cols] }

-- * Query

{- |
This is similar to Linq.
-}
data Query
    = QFrom Table -- ^ select all rows of the table
    | QProject [Col] Query -- ^ select some columns of subquery
    | QJoin Query Query
    | QFilter Exp Query -- ^ select only satisfying rows from subquery; the expression must be a boolean expression
    | QSkip Integer Query -- ^ skip first rows of subquery
    | QLimit Integer Query -- ^ take only first rows of subquery
    deriving (Read, Show)

-- | Perhaps we should introduce an intermediate module Meta.Sql?
renderSqlSelect :: Query -> String
renderSqlSelect q = case q of
    QFrom t ->
        -- TODO escape
        "SELECT * FROM " ++ maybe "" (++ ".") (_schema t) ++ tName t
    QProject cols subq ->
        "SELECT " ++ L.intercalate "," (map getName cols)
        ++ " FROM (" ++ renderSqlSelect subq ++ ") tmp"
    QFilter cond subq ->
        "SELECT * FROM (" ++ renderSqlSelect subq ++ ") tmp WHERE " ++ renderSqlExp cond
    _ ->
        error $ "Meta.Data_internal.renderSqlSelect: not implemented: " ++ show q
    where
        renderSqlExp :: Exp -> String
        renderSqlExp e = case e of
            ECol col -> getName col
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
    = ECol Col
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

-- * Column

-- | Database column name.
type DbColName = String

-- | Database column data type.
data Type
    = TInt32
    | TInt64
    | TVarChar Int -- ^ the limit is the maximum number of characters, not bytes
    deriving (Read, Show)

sql_type_name :: Type -> String
sql_type_name typ = case typ of
    TInt32 -> "INTEGER"
    TInt64 -> "BIGINT"
    TVarChar n -> "VARCHAR(" ++ show n ++ ")"
    _ -> error $ "Meta.Data_internal: sql_type_name: not implemented: " ++ show typ

-- * Column

-- | View table column title.
type ShortTitle = String

-- | Form field title.
type LongTitle = String

-- | Form field description.
type FormDesc = String

{- |
A 'Col' represents a database table column.
-}
data Col
    -- | Internal. Do not use. Use column constructors, getters, and setters.
    = MkCol {
        cType :: Type
        , cName :: DbColName
        , cShortTitle :: Maybe ShortTitle
        , cLongTitle :: Maybe LongTitle
        , cFormDesc :: Maybe FormDesc
        , _nullable :: Bool
    } deriving (Read, Show)

defCol :: Col
defCol = MkCol {
        cType = TInt32
        , cName = ""
        , cShortTitle = Nothing
        , cLongTitle = Nothing
        , cFormDesc = Nothing
        , _nullable = False
    }

-- * Constructors

mkCol :: Type -> DbColName -> Col
mkCol t n = defCol { cType = t, cName = n }

colInt32 :: DbColName -> Col
colInt32 name = defCol {
        cType = TInt32
        , cName = name
    }

colInt64 :: DbColName -> Col
colInt64 name = defCol {
        cType = TInt64
        , cName = name
    }

colVarChar :: Int -> DbColName -> Col
colVarChar limit name = defCol {
        cType = TVarChar limit
        , cName = name
    }

-- * Getters

getType :: Col -> Type
getType = cType

getName :: Col -> String
getName = cName

getShortTitle :: Col -> Maybe ShortTitle
getShortTitle = cShortTitle

getLongTitle :: Col -> Maybe LongTitle
getLongTitle = cShortTitle

-- * Setters

setShortTitle :: ShortTitle -> Col -> Col
setShortTitle t c = c { cShortTitle = Just t }

setLongTitle :: LongTitle -> Col -> Col
setLongTitle t c = c { cLongTitle = Just t }
