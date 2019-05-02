{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.Event
(
    -- * Make

    Event(..)
    , empty

    -- * Roll

    , roll

    -- * Modify

    , prob
    , setMessage
    , setEffect
)
where

import qualified Control.Monad as N
import qualified Control.Monad.IO.Class as I

import qualified Control.Monad.Trans.State as M

import qualified Dynasty.Random as R
import qualified Dynasty.Random.Uniform as U

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

roll :: (I.MonadIO m) => U.Uniform m U.Probability -> Event m -> m (Maybe (Event m))
roll unit event = do
    N.join $ U.bernoulli pr dud fire
    where
        pr = probability event
        dud = return Nothing
        fire = effect event >> return (Just event)

prob :: R.Probability -> Event m -> Event m
prob p e = e { probability = p }

empty :: (Applicative m) => Event m
empty = MkEvent 1 "" (pure ())
