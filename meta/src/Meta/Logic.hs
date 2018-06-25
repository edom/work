module Meta.Logic where

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

-- forall a1 a2 ...: p1, p2, ... |- q1, q2, ...
data Rule = MkRule {
        _rVars :: [String]
        , _rAnds :: [Exp Val]
        , _rOrs :: [Exp Val]
    } deriving (Read, Show)

type Program = [Rule]

type Context = [(Name, Val)]

-- forall a s: String s |- Html a s
example :: Rule
example = MkRule ["a", "s"] [sym "String" `app` [var "s"]] [sym "Html" `app` [var "a", var "s"]]
