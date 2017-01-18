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

        , diplomacy :: Int
        , martial :: Int
        , stewardship :: Int
        , intrigue :: Int
        , learning :: Int
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
    0 0 0 0 0

highestTitle :: Person -> Maybe T.Title
highestTitle p =
    case titles p of
        [] -> Nothing
        list -> Just $ maximum list

honorifiedName :: Person -> String
honorifiedName p =
    maybe name_ (\ title -> L.format (sex p) (T.level title) ++ " " ++ name_ ++ " of " ++ T.place title) $ highestTitle p
    where
        name_ = name p

type Today = D.Date

ageYear :: Today -> Person -> Int
ageYear today p =
    yt - yb - adjustment
    where
        (yt, mt, dt) = D.toYmd today
        (yb, mb, db) = D.toYmd (born p)
        birthdayPassed = (mt, dt) >= (mb, db)
        adjustment = if birthdayPassed then 0 else 1

formatLong :: Today -> Person -> String
formatLong today p =
    unlines $
        map indent $
            [
                pad "Id / Sex / Hon. Name:" ++ show (id p) ++ " / " ++ show (sex p) ++ " / " ++ honorifiedName p
                , pad "Religion / Culture:" ++ show (religion p) ++ " / " ++ show (culture p)
                , pad "Born / Age:" ++ D.print (born p) ++ " / " ++ show (ageYear today p) ++ " years (" ++ show ageDay ++ " days) old"
                , pad "Gold / Prestige / Piety:" ++ showd (gold p) ++ " / " ++ showd (prestige p) ++ " / " ++ showd (piety p)
                , pad "D / M / S / I / L:"
                    ++ show (diplomacy p) ++ " / " ++ show (martial p)
                    ++ " / " ++ show (stewardship p) ++ " / " ++ show (intrigue p)
                    ++ " / " ++ show (learning p)
                , "Titles:"
            ]
            ++ map ("    - " ++) (formattedTitlesOf p)
            ++
            [
                pad "Traits:" ++ show (traits p)
            ]
    where
        ageDay = today D.- born p
        showd x = show (floor x :: Int)
        minLength = 32
        pad string = string ++ replicate (minLength - length string) ' '
        indent = ("    " ++)

formattedTitlesOf :: Person -> [String]
formattedTitlesOf p = map (T.format $ sex p) $ titles p

increment :: Id -> Id
increment = (+) 1
