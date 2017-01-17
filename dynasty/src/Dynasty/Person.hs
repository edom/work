module Dynasty.Person where

import qualified Dynasty.Culture as C
import qualified Dynasty.Title as T

type Id = Int

data Person =
    MkPerson
    {
        id :: Int
        , name :: String
        , born :: Int
        , titles :: [T.Title]
        , died :: Maybe Int
        , culture :: C.Culture
    }
    deriving (Show)

empty :: Person
empty = MkPerson 0 "" 0 [] Nothing C.None

formattedTitlesOf :: Person -> [String]
formattedTitlesOf = map T.format . titles

increment :: Id -> Id
increment = (+) 1
