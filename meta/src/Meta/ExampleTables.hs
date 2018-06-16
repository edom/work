module Meta.ExampleTables where

import Meta.User ((|>))

import qualified Meta.User as U

-- | All tables.
tables :: [U.Table]
tables = [
        customer
        , sku
        , order
    ]

customer :: U.Table
customer = U.mk_table "customer" [
        c_id64
        , U.col_varchar 128 "name" |> U.col_set_title "Name"
    ]
    |> U.tab_add_primary_key [c_id64]

order :: U.Table
order = U.mk_table "order" [
        c_id64
        , U.col_varchar 16 "ref_num"
            |> U.col_set_short_title "RefNum"
            |> U.col_set_long_title "Reference number"
        , U.col_int64 "customer_id"
        , U.col_int64 "sku_id"
    ]
    |> U.tab_add_primary_key [c_id64]

sku :: U.Table
sku = U.mk_table "sku" [
        c_id64
        , U.col_varchar 16 "code"
        , U.col_varchar 128 "name"
    ]
    |> U.tab_add_primary_key [c_id64]

c_id64 :: U.Column
c_id64 = U.col_int64 "id"
