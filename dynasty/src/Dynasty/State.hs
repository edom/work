module Dynasty.State where

import qualified Dynasty.Person as P

data State =
    MkState
    {
        day :: Int
        , people :: [P.Person]
    }
    deriving (Show)

incrementDate :: State -> State
incrementDate x = x { day = day x + 1 }

initialState :: State
initialState = MkState 0 []
