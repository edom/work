{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.Event where

import qualified Control.Monad.Trans.State as M

import qualified Dynasty.Person as P
import qualified Dynasty.State as S

type State = S.State

newtype EventM a = MkEventM (State -> (Either String a, State))

run :: EventM a -> State -> (Either String a, State)
run (MkEventM x) = x

mapfst :: (a -> c) -> (a, b) -> (c, b)
mapfst f (x, y) = (f x, y)

instance Functor EventM where
    fmap f (MkEventM k) = MkEventM $ \ s -> mapfst (fmap f) (k s)

instance Applicative EventM where
    pure x = MkEventM $ \ s -> (Right x, s)
    (<*>) ff fx = do
        f <- ff
        x <- fx
        return $ f x

instance Monad EventM where
    return = pure
    (>>=) m k = MkEventM $ \ s ->
        let
            (ea, t) = run m s
            (eb, u) = either (\ x -> (Left x, t)) (\ a -> run (k a) t) ea
        in
            (eb, u)
    fail msg = MkEventM $ \ s -> (Left msg, s)

bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap f _ (Left x) = Left (f x)
bimap _ g (Right x) = Right (g x)

findPerson :: P.Id -> EventM P.Person
findPerson id = do
    people <- gets S.people
    case filter (\ p -> P.id p == id) people of
        x : _ -> return x
        _ -> fail $ "This game does not have a person with id " ++ show id

embed :: EventM a -> S.StateM (Either String a)
embed m = M.state $ \ s -> run m s

lift :: S.StateM a -> EventM a
lift m = MkEventM $ \ s ->
        mapfst Right $ M.runState m s

get :: EventM State
get = lift M.get

gets :: (State -> s) -> EventM s
gets = lift . M.gets

exec :: EventM a -> State -> State
exec m s = snd $ run m s

eval :: EventM a -> State -> Either String a
eval m s = fst $ run m s
