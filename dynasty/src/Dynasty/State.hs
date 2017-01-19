{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.State
(
    State(..)

    -- * Make

    , empty
    , incrementDate
    , findPerson
    , modifyPerson

    -- * Internal

    , Marriage(..)
    , incrementPersonId
)
where

import Prelude hiding (print)

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

        , marriages :: [Marriage]
    }

data Marriage
    = MkMarriage
    {
        begin :: D.Date
        , end :: Maybe D.Date
        , husband :: P.Id
        , wife :: P.Id
    }

incrementDate :: State -> State
incrementDate x = x { today = D.increment $ today x }

empty :: State
empty =
    MkState (D.fromYmd 1066 1 1) 0 [] 0
    []

findPerson :: P.Id -> State -> [P.Person]
findPerson id state =
    filter (\ p -> P.id p == id) $ people state

modifyPerson :: P.Id -> (P.Person -> P.Person) -> State -> State
modifyPerson id fun state =
    state { people = map (\ p -> if P.id p == id then fun p else p) (people state) }

incrementPersonId :: State -> State
incrementPersonId state = state { nextPersonId = P.increment (nextPersonId state) }
