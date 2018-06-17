module Meta.ExampleTables where

import Meta.User ((|>))

import qualified Meta.User as U

mk_table :: U.Table_name -> [U.Column] -> U.Table
mk_table name cols =
    U.mk_table name cols
    |> U.tab_set_schema "example"
    |> U.tab_set_DataSource_field_name "ds_test"

-- | All tables.
tables :: [U.Table]
tables = [
        customer
        , sku
        , order
    ]

customer :: U.Table
customer = mk_table "customer" [
        c_id64
        , U.col_varchar 128 "name" |> U.col_set_title "Name"
    ]
    |> U.tab_add_primary_key [c_id64]

order :: U.Table
order = mk_table "order" [
        c_id64
        , U.col_varchar 16 "ref_num"
            |> U.col_set_short_title "RefNum"
            |> U.col_set_long_title "Reference number"
        , U.col_int64 "customer_id"
        , U.col_int64 "sku_id"
    ]
    |> U.tab_add_primary_key [c_id64]

sku :: U.Table
sku = mk_table "sku" [
        c_id64
        , U.col_varchar 16 "code"
        , U.col_varchar 128 "name"
    ]
    |> U.tab_add_primary_key [c_id64]

c_id64 :: U.Column
c_id64 = U.col_int64 "id"
