module Meta.ExampleTables where

import Meta.Prop ((|>))

import qualified Meta.Relat as R

-- | All tables.
tables :: [R.Table]
tables = [
        customer
        , sku
        , order
    ]

customer :: R.Table
customer = R.mkTable "customer" [
        c_id64
        , R.colVarChar 128 "name"
    ]
    |> R.addPrimaryKey [c_id64]

order :: R.Table
order = R.mkTable "order" [
        c_id64
        , R.colVarChar 16 "ref_num"
        , R.colInt64 "customer_id"
        , R.colInt64 "sku_id"
    ]
    |> R.addPrimaryKey [c_id64]

sku :: R.Table
sku = R.mkTable "sku" [
        c_id64
        , R.colVarChar 16 "code"
        , R.colVarChar 128 "name"
    ]
    |> R.addPrimaryKey [c_id64]

c_id64 :: R.Column
c_id64 = R.colInt64 "id"
