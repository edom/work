module Meta.PegGenTree (
    Tree(..)
) where

import qualified Meta.PegGenRule as R

data State t
    = MkState {
        _sGram :: R.Grammar t
        , _sTokens :: [t]
    } deriving (Read, Show)

from :: R.Grammar t -> [t] -> State t
from = MkState

data Tree t a
    = Nil
    | Term t
    | Seq a a
    | Call R.Name a
    deriving (Read, Show)
