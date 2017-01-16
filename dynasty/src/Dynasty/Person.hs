module Dynasty.Person where

import qualified Dynasty.Title as T

data Person =
    MkPerson
    {
        name :: String
        , born :: Int
        , titles :: [T.Title]
        , died :: Maybe Int
    }
    deriving (Show)

empty :: Person
empty = MkPerson "" 0 [] Nothing

formattedTitlesOf :: Person -> [String]
formattedTitlesOf = map T.format . titles
