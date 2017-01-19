{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.Event
(
    -- * Make

    Event(..)
    , empty

    , addDiplomacy
    , addStewardship

    -- * Roll

    , roll

    -- * Modify

    , prob
    , setMessage
    , setEffect
)
where

import qualified Control.Monad as N

import qualified Control.Monad.Trans.State as M

import qualified Dynasty.Person as P
import qualified Dynasty.Person.Modify as Q
import qualified Dynasty.Random as R
import qualified Dynasty.State as S
import qualified Dynasty.State.Monad as SM

data Event m
    = MkEvent
    {
        probability :: R.Probability
        ,
        -- | The player will see this.
        message :: String
        ,
        -- | This is how the event changes the game.
        effect :: m ()
    }

setMessage :: String -> Event m -> Event m
setMessage str e = e { message = str }

setEffect :: m () -> Event m -> Event m
setEffect eff e = e { effect = eff }

roll :: (R.MonadRandom m) => Event m -> m (Maybe (Event m))
roll event = do
    N.join $ R.bernoulli pr dud fire
    where
        pr = probability event
        dud = return Nothing
        fire = effect event >> return (Just event)

prob :: R.Probability -> Event m -> Event m
prob p e = e { probability = p }

empty :: (Applicative m) => Event m
empty = MkEvent 1 "" (pure ())

addDiplomacy :: (SM.MonadState m) => Int -> P.Person -> Event m
addDiplomacy point person = MkEvent 1 msg eff
    where
        msg = P.honorifiedName person ++ " " ++ verb ++ " " ++ show (abs point) ++ " Diplomacy"
        verb = if point >= 0 then "gains" else "loses"
        eff = SM.modifyPerson (P.id person) $ Q.addDiplomacy point

addStewardship :: (SM.MonadState m) => Int -> P.Person -> Event m
addStewardship point person = MkEvent 1 msg eff
    where
        msg = P.honorifiedName person ++ " " ++ verb ++ " " ++ show (abs point) ++ " Stewardship"
        verb = if point >= 0 then "gains" else "loses"
        eff = SM.modifyPerson (P.id person) $ Q.addStewardship point

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

embed :: EventM a -> SM.StateM (Either String a)
embed m = M.state $ \ s -> run m s

lift :: SM.StateM a -> EventM a
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
