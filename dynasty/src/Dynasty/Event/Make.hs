module Dynasty.Event.Make
(
    addDiplomacy
    , addStewardship
)
where

import qualified Dynasty.Event as E
import qualified Dynasty.Person as P
import qualified Dynasty.Person.Modify as Q
import qualified Dynasty.State as S
import qualified Dynasty.Stateful as T

addDiplomacy :: (Monad m) => T.Stateful S.State m -> Int -> P.Person -> E.Event m
addDiplomacy inst point person = E.MkEvent 1 msg eff
    where
        msg = P.honorifiedName person ++ " " ++ verb ++ " " ++ show (abs point) ++ " Diplomacy"
        verb = if point >= 0 then "gains" else "loses"
        eff = T.modify inst $ S.modifyPerson (P.id person) $ Q.addDiplomacy point

addStewardship :: (Monad m) => T.Stateful S.State m -> Int -> P.Person -> E.Event m
addStewardship inst point person = E.MkEvent 1 msg eff
    where
        msg = P.honorifiedName person ++ " " ++ verb ++ " " ++ show (abs point) ++ " Stewardship"
        verb = if point >= 0 then "gains" else "loses"
        eff = T.modify inst $ S.modifyPerson (P.id person) $ Q.addStewardship point
