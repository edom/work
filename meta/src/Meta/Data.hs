module Meta.Data (
    Table
    , Col
    , I.Column
    , Col_name
    , Table_name
    , Type
    , Field_name
    , t_DataSource_field_name
    , t_set_DataSource_field_name
    , c_get_type
    , c_get_name
    , c_get_nullable
    , c_short_title
    , c_long_title
    , Constraint
    , t_get_name
    , t_get_cols
    , t_get_constraints
    , t_get_schema
    , I.t_set_schema
    -- * Query
    , Q.Query(..)
) where

import qualified Meta.Data_internal as I
import qualified Meta.DataQuery as Q

-- * Table

type Table = I.Table

type Col = I.Col

type Col_name = I.DbColName

type Table_name = I.DbTabName

type Type = I.Type

type Field_name = String

t_DataSource_field_name :: Table -> Maybe Field_name
t_DataSource_field_name = I._t_ds_field

t_set_DataSource_field_name ::Field_name -> Table -> Table
t_set_DataSource_field_name fld tab = tab { I._t_ds_field = Just fld }

c_get_type :: Col -> Type
c_get_type = I.getType

c_get_name :: Col -> Col_name
c_get_name = I.getName

c_get_nullable :: Col -> Bool
c_get_nullable = I._cNullable

c_short_title :: Col -> Maybe String
c_short_title = I.cShortTitle

c_long_title :: Col -> Maybe String
c_long_title = I.cLongTitle

type Constraint = I.Constraint

t_get_name :: Table -> Table_name
t_get_name = I.tName

t_get_cols :: Table -> [Col]
t_get_cols = I.tCols

t_get_constraints :: Table -> [Constraint]
t_get_constraints = I.tConstraints

t_get_schema :: Table -> Maybe String
t_get_schema = I._schema
