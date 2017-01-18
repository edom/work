{-# LANGUAGE FlexibleInstances #-}

module Dynasty.State.Monad
(
    -- * Class

    MonadState(..)

    -- * State monad

    , StateM
    , exec
    , eval

    -- * On the monad

    , newPersonWith
    , marry
    , today
)
where

import Prelude hiding (id, init)

import qualified Control.Monad.Trans.State as M

import qualified Dynasty.Date as D
import qualified Dynasty.Person as P
import qualified Dynasty.State as S

class (Monad m) => MonadState m where

    get :: m S.State

    modify :: (S.State -> S.State) -> m ()

instance (Monad m) => MonadState (M.StateT S.State m) where

    get = M.get

    modify = M.modify

type StateM = M.State S.State

gets :: (MonadState m) => (S.State -> s) -> m s
gets f = f <$> get

exec :: StateM a -> S.State -> S.State
exec = M.execState

eval :: StateM a -> S.State -> a
eval = M.evalState

newPerson :: (MonadState m) => m P.Person
newPerson = P.newWithId <$> newPersonId

newPersonId :: (MonadState m) => m P.Id
newPersonId = do
    id <- gets S.nextPersonId
    modify S.incrementPersonId
    return id

{- |
Create a new 'P.Person', initialize it,
and add it to the game.

Return the person.
-}
newPersonWith :: (MonadState m) => (P.Person -> P.Person) -> m P.Person
newPersonWith init = do
    p <- init <$> newPerson
    addPerson p
    return p

addPerson :: (MonadState m) => P.Person -> m ()
addPerson p = do
    modify $ \ s -> s { S.people = p : S.people s }

today :: (MonadState m) => m D.Date
today = gets S.today

marry :: (MonadState m) => P.Person -> P.Person -> m S.Marriage
marry p q = do
    t <- today
    let m = S.MkMarriage t Nothing (P.id p) (P.id q)
    modify $ \ s -> s { S.marriages = m : S.marriages s }
    return m
