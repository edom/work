module Dynasty.Server
(
    -- * Type

    Server(..)

    -- * Make

    , fromStateful
)
where

import qualified Dynasty.Date as D
import qualified Dynasty.Person as P
import qualified Dynasty.State as S
import qualified Dynasty.Stateful as T

{- |
An inhabitant of this type hides the game state and enforces the game rules.
-}
data Server m
    = MkServer
    {
        getToday :: m D.Date
        , getPeople :: m [P.Person]
        , endDay :: m ()
    }

fromStateful :: (Monad m) => T.Stateful S.State m -> Server m
fromStateful c = MkServer
    (T.gets c S.today)
    (T.gets c S.people)
    (T.modify c S.incrementDate)
