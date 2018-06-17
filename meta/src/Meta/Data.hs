module Meta.Data where

import qualified Meta.Data_internal as I

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
c_get_nullable = I._nullable

c_short_title :: Col -> Maybe String
c_short_title = I.cShortTitle

type Constraint = I.Constraint

t_get_name :: Table -> Table_name
t_get_name = I.tName

t_get_cols :: Table -> [Col]
t_get_cols = I.tCols

t_get_constraints :: Table -> [Constraint]
t_get_constraints = I.tConstraints

t_get_schema :: Table -> Maybe String
t_get_schema = I._schema

t_set_schema :: String -> Table -> Table
t_set_schema s t = t { I._schema = Just s }

-- * Query

type Query = I.Query
