module Parse.Location
(
    Location(..)
    , Path
)
where

type Path = String

data Location
    = MkLocation
    {
        path :: Path
        , line :: Int
        , column :: Int
    }
    deriving (Read, Show)
