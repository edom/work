module Meta.Data_internal where

import qualified Meta.DataColumn as C

data Table
    -- | Internal. Do not use. Use 'defTable'.
    = MkTable {
        tName :: DbTabName
        , tCols :: [C.Column]
        , tConstraints :: [Constraint]
        , _schema :: Maybe String
        , _t_ds_field :: Maybe String
    } deriving (Read, Show)

type Table_name = DbTabName

mk_table :: Table_name -> [C.Column] -> Table
mk_table = mkTable

set_schema :: String -> Table -> Table
set_schema = t_set_schema

t_set_schema :: String -> Table -> Table
t_set_schema s t = t { _schema = Just s }

data Constraint
    = KPrimaryKey [C.Column]
    | KForeignKey [C.Column] Table [C.Column]
    deriving (Read, Show)

-- | Database table name.
type DbTabName = String

mkTable :: DbTabName -> [C.Column] -> Table
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

addPrimaryKey :: [C.Column] -> Table -> Table
addPrimaryKey cols tab = tab { tConstraints = tConstraints tab ++ [KPrimaryKey cols] }

t_get_name :: Table -> Table_name
t_get_name = tName

t_get_cols :: Table -> [C.Column]
t_get_cols = tCols

t_get_constraints :: Table -> [Constraint]
t_get_constraints = tConstraints

t_get_schema :: Table -> Maybe String
t_get_schema = _schema

type Field_name = String

t_DataSource_field_name :: Table -> Maybe Field_name
t_DataSource_field_name = _t_ds_field

t_set_DataSource_field_name ::Field_name -> Table -> Table
t_set_DataSource_field_name fld tab = tab { _t_ds_field = Just fld }

add_primary_key :: [C.Column] -> Table -> Table
add_primary_key = addPrimaryKey
