module Dynasty.Person
(
    -- * Record

    Person(..)

    -- * Make

    , empty
    , newWithId

    -- * Textual display

    , honorifiedName
    , formatLong
    , highestTitle

    -- * Identifier

    , Id

    , increment

    -- * Age

    , Today

    , ageYear

    , Marriage(..)
)
where

import Prelude hiding (id)

import qualified Dynasty.Culture as C
import qualified Dynasty.Date as D
import qualified Dynasty.Level as L
import qualified Dynasty.Religion as R
import qualified Dynasty.Title as T
import qualified Dynasty.Trait as U

{- |
The game should not have more than one million people.
-}
type Id = Int

{- |
An inhabitant of this represents a person in the game.

If you are thinking about using 'MkPerson', use 'empty' instead.
-}
data Person =
    MkPerson
    {
        id :: Id
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

{- |
This frees you from having to memorize the order of parameters of 'MkPerson'.

This allows us to add and reorder parameters of 'MkPerson' without breaking your code.
-}
empty :: Person
empty =
    MkPerson 0 "" (D.fromYmd 1066 1 1) [] Nothing C.None R.None [] L.Male []
    0 0 0
    0 0 0 0 0

newWithId :: Id -> Person
newWithId x = empty { id = x }

{- |
Compute the highest title held by a person, if any.
-}
highestTitle :: Person -> Maybe T.Title
highestTitle p =
    case titles p of
        [] -> Nothing
        list -> Just $ maximum list

{- |
Given a person named Man whose 'highestTitle' is the 'L.King' of Kingdom,
this gives the string King Man of Kingdom.
-}
honorifiedName :: Person -> String
honorifiedName p =
    maybe name_ (\ title -> L.format (sex p) (T.level title) ++ " " ++ name_ ++ " of " ++ T.place title) $ highestTitle p
    where
        name_ = name p

type Today = D.Date

{- |
Compute how old, in years, someone would be at the given date, accounting for birthday.
-}
ageYear :: Today -> Person -> Int
ageYear today p =
    yt - yb - adjustment
    where
        (yt, mt, dt) = D.toYmd today
        (yb, mb, db) = D.toYmd (born p)
        birthdayPassed = (mt, dt) >= (mb, db)
        adjustment = if birthdayPassed then 0 else 1

{- |
Format the person for multiline textual display.
-}
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
