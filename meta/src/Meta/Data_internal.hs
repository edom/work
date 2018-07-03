module Meta.Data_internal where

import qualified Meta.SqlType as T

type Type = T.Type

data Table
    -- | Internal. Do not use. Use 'defTable'.
    = MkTable {
        tName :: DbTabName
        , tCols :: [Col]
        , tConstraints :: [Constraint]
        , _schema :: Maybe String
        , _t_ds_field :: Maybe String
    } deriving (Read, Show)

type Table_name = String

mk_table :: Table_name -> [Column] -> Table
mk_table = mkTable

set_schema :: String -> Table -> Table
set_schema = t_set_schema

t_set_schema :: String -> Table -> Table
t_set_schema s t = t { _schema = Just s }

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

-- * Column

-- | Database column name.
type DbColName = String

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
        , _cNullable :: Bool
        , _cAutoIncrement :: Bool
    } deriving (Read, Show)

type Column = Col

defCol :: Col
defCol = MkCol {
        cType = T.Int32
        , cName = ""
        , cShortTitle = Nothing
        , cLongTitle = Nothing
        , cFormDesc = Nothing
        , _cNullable = False
        , _cAutoIncrement = False
    }

type Column_name = String

col_varchar :: Int -> Column_name -> Column
col_varchar = colVarChar

col_int32 :: Column_name -> Column
col_int32 = colInt32

col_int64 :: Column_name -> Column
col_int64 = colInt64

col_numeric :: Int -> Int -> Column_name -> Column
col_numeric precision scale = mkCol (T.Numeric precision scale)

col_boolean :: Column_name -> Column
col_boolean = mkCol T.Boolean

type Short_title = String

type Long_title = String

set_short_title :: Short_title -> Column -> Column
set_short_title = setShortTitle

set_long_title :: Long_title -> Column -> Column
set_long_title = setLongTitle

set_title :: String -> Column -> Column
set_title t = set_long_title t . set_short_title t

set_titles :: Short_title -> Long_title -> Column -> Column
set_titles st lt = set_long_title lt . set_short_title st

add_primary_key :: [Column] -> Table -> Table
add_primary_key = addPrimaryKey

-- * Constructors

mkCol :: Type -> DbColName -> Col
mkCol t n = defCol { cType = t, cName = n }

colInt32 :: DbColName -> Col
colInt32 name = defCol {
        cType = T.Int32
        , cName = name
    }

colInt64 :: DbColName -> Col
colInt64 name = defCol {
        cType = T.Int64
        , cName = name
    }

colVarChar :: Int -> DbColName -> Col
colVarChar limit name = defCol {
        cType = T.VarChar limit
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
