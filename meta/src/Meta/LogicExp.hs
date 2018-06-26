module Meta.LogicExp where

import qualified Meta.LogicVal as V

type Val = V.Val

type Name = V.Name

data Exp a
    = Pure a
    | App (Exp a) [Exp a]
    deriving (Read, Show)

sym :: String -> Exp Val
sym = Pure . V.Sym

app :: Exp a -> [Exp a] -> Exp a
app = App

var :: Name -> Exp Val
var = Pure . V.Var
