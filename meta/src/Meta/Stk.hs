{- |
Stack machine.
-}
module Meta.Stk (
    Prog
    , Ins(..)
) where

type Prog a = [Ins a]

data Ins a
    = Push a
    | Pop
    | Add
    deriving (Read, Show)
