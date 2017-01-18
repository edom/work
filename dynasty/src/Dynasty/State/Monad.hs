module Dynasty.State.Monad
(
    -- * State monad

    StateM
    , exec
    , eval
    , gets
    , modify

    -- * On the monad

    , newPersonWith
)
where

import Prelude hiding (id, init)

import qualified Control.Monad.Trans.State as M

import qualified Dynasty.Person as P
import qualified Dynasty.State as S

type StateM = M.State S.State

exec :: StateM a -> S.State -> S.State
exec = M.execState

eval :: StateM a -> S.State -> a
eval = M.evalState

gets :: (S.State -> s) -> StateM s
gets = M.gets

modify :: (S.State -> S.State) -> StateM ()
modify = M.modify

newPerson :: StateM P.Person
newPerson = P.newWithId <$> newPersonId

newPersonId :: StateM P.Id
newPersonId = do
    id <- gets S.nextPersonId
    modify S.incrementPersonId
    return id

{- |
Create a new 'P.Person', initialize it,
and add it to the game.
-}
newPersonWith :: (P.Person -> P.Person) -> StateM ()
newPersonWith init = do
    newPerson >>= addPerson . init

addPerson :: P.Person -> StateM ()
addPerson p = do
    modify $ \ s -> s { S.people = p : S.people s }
