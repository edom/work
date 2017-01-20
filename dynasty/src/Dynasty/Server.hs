{-# LANGUAGE NoMonomorphismRestriction #-}

module Dynasty.Server
(
    -- * Type

    Server(..)

    -- * Make

    , fromStateful

    , makeEventsFor
)
where

import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as I
import qualified Data.Maybe as N

import qualified Dynasty.Date as D
import qualified Dynasty.Event as G
import qualified Dynasty.Event.Make as EM
import qualified Dynasty.Person as P
import qualified Dynasty.Random.Uniform as U
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

{- |
If you have a @'T.Stateful' 'S.State' m@,
you can get a @'Server' m@ for free.
-}
fromStateful :: (I.MonadIO m) => T.Stateful S.State m -> Server m
fromStateful c = MkServer
    (T.gets c S.today)
    getPeople_
    endDay_
    where
        getPeople_ = T.gets c S.people
        endDay_ = do
            T.modify c S.incrementDate
            people <- getPeople_
            events <- makeEventsFor c people
            happenings <- N.catMaybes <$> M.mapM (G.roll U.unit) events
            return ()

makeEventsFor :: (Monad m) => T.Stateful S.State m -> [P.Person] -> m [G.Event m]
makeEventsFor inst people =
    return $ personalEvents ++ pairEvents
    where
        addDiplomacy = EM.addDiplomacy inst
        addStewardship = EM.addStewardship inst
        personalEvents = flip concatMap people $ \ p ->
            [
                G.prob (1/4) $ addDiplomacy 1 p
                , G.prob (1/4) $ addStewardship 1 p
            ]
        pairs = [ (p, q) | p <- people, q <- people, P.id p /= P.id q ]
        pairEvents = flip concatMap pairs $ \ (p, q) ->
            let
                i = P.id p
                j = P.id q
            in
                [
                    -- G.prob (1/16)
                    -- . G.setMessage ("Character " ++ show i ++ "'s opinion of character " ++ show j ++ " improves by 10 until 1066-01-01. (Not implemented.)")
                    -- -- $ G.empty
                ]
