module Meta.DataColumn where

import qualified Meta.SqlType as T

-- | Database column name.
type DbColName = String

type Col_name = DbColName

-- | View table column title.
type ShortTitle = String

-- | Form field title.
type LongTitle = String

-- | Form field description.
type FormDesc = String

{- |
A 'Column' represents a database table column.
-}
data Column
    -- | Internal. Do not use. Use column constructors, getters, and setters.
    = MkColumn {
        cType :: T.Type
        , cName :: DbColName
        , cShortTitle :: Maybe ShortTitle
        , cLongTitle :: Maybe LongTitle
        , cFormDesc :: Maybe FormDesc
        , _cNullable :: Bool
        , _cAutoIncrement :: Bool
    } deriving (Read, Show)

type Col = Column

defCol :: Column
defCol = MkColumn {
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

get_auto_increment :: Column -> Bool
get_auto_increment = _cAutoIncrement

set_auto_increment :: Bool -> Column -> Column
set_auto_increment b c = c { _cAutoIncrement = b }

set_nullable :: Bool -> Column -> Column
set_nullable b c = c { _cNullable = b }

-- * Constructors

mkCol :: T.Type -> DbColName -> Column
mkCol t n = defCol { cType = t, cName = n }

colInt32 :: DbColName -> Column
colInt32 name = defCol {
        cType = T.Int32
        , cName = name
    }

colInt64 :: DbColName -> Column
colInt64 name = defCol {
        cType = T.Int64
        , cName = name
    }

colVarChar :: Int -> DbColName -> Column
colVarChar limit name = defCol {
        cType = T.VarChar limit
        , cName = name
    }

-- * Getters

getType :: Column -> T.Type
getType = cType

getName :: Column -> String
getName = cName

getShortTitle :: Column -> Maybe ShortTitle
getShortTitle = cShortTitle

getLongTitle :: Column -> Maybe LongTitle
getLongTitle = cShortTitle

-- * Setters

setShortTitle :: ShortTitle -> Column -> Column
setShortTitle t c = c { cShortTitle = Just t }

setLongTitle :: LongTitle -> Column -> Column
setLongTitle t c = c { cLongTitle = Just t }

c_get_type :: Column -> T.Type
c_get_type = getType

c_get_name :: Column -> Col_name
c_get_name = getName

c_get_nullable :: Column -> Bool
c_get_nullable = _cNullable

c_short_title :: Column -> Maybe String
c_short_title = cShortTitle

c_long_title :: Column -> Maybe String
c_long_title = cLongTitle
