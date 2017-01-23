module Parse.Location
(
    Location(..)
)
where

data Location
    = MkLocation
    {
        path :: String
        , line :: Int
        , column :: Int
    }
