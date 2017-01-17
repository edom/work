module Dynasty.Person where

import Prelude hiding (id)

import qualified Dynasty.Culture as C
import qualified Dynasty.Religion as R
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
        , religion :: R.Religion
    }
    deriving (Show)

empty :: Person
empty = MkPerson 0 "" 0 [] Nothing C.None R.None

type Today = Int

formatLong :: Today -> Person -> String
formatLong today p =
    unlines $
        map indent $
            [
                pad "Id / Name:" ++ show (id p) ++ " / " ++ name p
                , pad "Religion / Culture:" ++ show (religion p) ++ " / " ++ show (culture p)
                , pad "Born / Age:" ++ "Day " ++ show (born p) ++ " / " ++ show (today - born p) ++ " days old"
                , "Titles:"
            ]
            ++ map ("    - " ++ ) (formattedTitlesOf p)
    where
        minLength = 24
        pad string = string ++ replicate (minLength - length string) ' '
        indent = ("    " ++)

formattedTitlesOf :: Person -> [String]
formattedTitlesOf = map T.format . titles

increment :: Id -> Id
increment = (+) 1
