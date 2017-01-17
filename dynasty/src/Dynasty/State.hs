{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.State where

import qualified Control.Monad.Trans.State as M

import qualified Dynasty.Person as P

data State =
    MkState
    {
        day :: Int
        , nextPersonId :: P.Id
        , people :: [P.Person]
    }
    deriving (Show)

incrementDate :: State -> State
incrementDate x = x { day = day x + 1 }

empty :: State
empty = MkState 0 0 []

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
