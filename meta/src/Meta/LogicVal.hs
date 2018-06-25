module Meta.LogicVal where

type Name = String

data Val
    = Sym String
    | Str String
    | Var Name
    deriving (Read, Show)

