module Dynasty.Person where

import Prelude hiding (id)

import qualified Dynasty.Culture as C
import qualified Dynasty.Religion as R
import qualified Dynasty.Title as T
import qualified Dynasty.Trait as U

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
        , traits :: [U.Trait]
        , sex :: T.Sex
    }
    deriving (Show)

empty :: Person
empty = MkPerson 0 "" 0 [] Nothing C.None R.None [] T.Male

type Today = Int

formatLong :: Today -> Person -> String
formatLong today p =
    unlines $
        map indent $
            [
                pad "Id / Sex / Name:" ++ show (id p) ++ " / " ++ show (sex p) ++ " / " ++ name p
                , pad "Religion / Culture:" ++ show (religion p) ++ " / " ++ show (culture p)
                , pad "Born / Age:" ++ "Day " ++ show (born p) ++ " / " ++ show (today - born p) ++ " days old"
                , "Titles:"
            ]
            ++ map ("    - " ++) (formattedTitlesOf p)
            ++
            [
                pad "Traits:" ++ show (traits p)
            ]
    where
        minLength = 24
        pad string = string ++ replicate (minLength - length string) ' '
        indent = ("    " ++)

formattedTitlesOf :: Person -> [String]
formattedTitlesOf p = map (T.format $ sex p) $ titles p

increment :: Id -> Id
increment = (+) 1
