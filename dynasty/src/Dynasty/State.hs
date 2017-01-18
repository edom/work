{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.State
(
    State(..)

    -- * Make

    , empty
    , incrementDate
    , findPeople
    , modifyPeople

    -- * Display

    , print

    -- * Internal

    , incrementPersonId
)
where

import Prelude hiding (print)

import qualified Data.List as L

import qualified Dynasty.Date as D
import qualified Dynasty.Person as P

{- |
An inhabitant of this type is a snapshot of the game at a given moment.
-}
data State =
    MkState
    {
        today :: D.Date
        , nextPersonId :: P.Id
        , people :: [P.Person]

        , playerCharId :: P.Id
    }

incrementDate :: State -> State
incrementDate x = x { today = D.increment $ today x }

empty :: State
empty = MkState (D.fromYmd 1066 1 1) 0 [] 0

findPeople :: P.Id -> State -> [P.Person]
findPeople id state =
    filter (\ p -> P.id p == id) $ people state

modifyPeople :: P.Id -> (P.Person -> P.Person) -> State -> State
modifyPeople id fun state =
    state { people = map (\ p -> if P.id p == id then fun p else p) (people state) }

print :: State -> String
print state =
    "You are playing as " ++ show playerCharId_ ++ " [" ++ L.intercalate ", " playerCharNames ++ "]\n"
    where
        playerCharId_ = playerCharId state
        playerCharNames = map P.honorifiedName myChars
        myChars = findPeople playerCharId_ state

incrementPersonId :: State -> State
incrementPersonId state = state { nextPersonId = P.increment (nextPersonId state) }
