{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.State where

import qualified Control.Monad.Trans.State as M

import qualified Dynasty.Date as D
import qualified Dynasty.Person as P

data State =
    MkState
    {
        today :: D.Date
        , nextPersonId :: P.Id
        , people :: [P.Person]
    }

incrementDate :: State -> State
incrementDate x = x { today = D.increment $ today x }

empty :: State
empty = MkState (D.fromYmd 1066 1 1) 0 []

type StateM = M.State State

exec :: StateM a -> State -> State
exec = M.execState

eval :: StateM a -> State -> a
eval = M.evalState

gets :: (State -> s) -> StateM s
gets = M.gets

modify :: (State -> State) -> StateM ()
modify = M.modify

newPerson :: StateM P.Person
newPerson = do
    id <- gets nextPersonId
    modify $ \ s -> s { nextPersonId = P.increment id }
    return $ P.empty { P.id = id }

newPersonWith :: (P.Person -> P.Person) -> StateM ()
newPersonWith init = do
    newPerson >>= addPerson . init

addPerson :: P.Person -> StateM ()
addPerson p = do
    modify $ \ s -> s { people = p : people s }
