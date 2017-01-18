module Dynasty.Person where

import Prelude hiding (id)

import qualified Dynasty.Culture as C
import qualified Dynasty.Date as D
import qualified Dynasty.Level as L
import qualified Dynasty.Religion as R
import qualified Dynasty.Title as T
import qualified Dynasty.Trait as U

type Id = Int

data Person =
    MkPerson
    {
        id :: Int
        , name :: String
        , born :: D.Date
        , titles :: [T.Title]
        , died :: Maybe D.Date
        , culture :: C.Culture
        , religion :: R.Religion
        , traits :: [U.Trait]
        , sex :: L.Sex
        , marriages :: [Marriage]

        , gold :: Double
        , prestige :: Double
        , piety :: Double
    }

data Marriage
    = MkMarriage
    {
        husband :: Id
        , wife :: Id
    }
    deriving (Show)

empty :: Person
empty =
    MkPerson 0 "" (D.fromYmd 1066 1 1) [] Nothing C.None R.None [] L.Male []
    0 0 0

type Today = D.Date

formatLong :: Today -> Person -> String
formatLong today p =
    unlines $
        map indent $
            [
                pad "Id / Sex / Name:" ++ show (id p) ++ " / " ++ show (sex p) ++ " / " ++ name p
                , pad "Religion / Culture:" ++ show (religion p) ++ " / " ++ show (culture p)
                , pad "Born / Age:" ++ D.print (born p) ++ " / " ++ show (today D.- born p) ++ " days old"
                , pad "Gold / Prestige / Piety:" ++ showd (gold p) ++ " / " ++ showd (prestige p) ++ " / " ++ showd (piety p)
                , "Titles:"
            ]
            ++ map ("    - " ++) (formattedTitlesOf p)
            ++
            [
                pad "Traits:" ++ show (traits p)
            ]
    where
        showd x = show (floor x :: Int)
        minLength = 32
        pad string = string ++ replicate (minLength - length string) ' '
        indent = ("    " ++)

formattedTitlesOf :: Person -> [String]
formattedTitlesOf p = map (T.format $ sex p) $ titles p

increment :: Id -> Id
increment = (+) 1
