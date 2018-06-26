{-# LANGUAGE RecordWildCards #-}

module Meta.Logic where

import qualified Meta.LogicExp as E

type Val = E.Val

type Exp = E.Exp E.Val

type Name = E.Name

-- forall a1 a2 ...: p1, p2, ... |- q1, q2, ...;
data Rule = MkRule {
        rAnte :: [Exp] -- ^ and-ed
        , rCons :: [Exp] -- ^ or-ed
    } deriving (Read, Show)

data Binding = MkBinding {
        cName :: Name
        , cExp :: Exp
    } deriving (Read, Show)

type Answer = [Binding]

-- forall a s: String s |- Html a s;
-- forall: |- String(var_n);
example :: [Rule]
example = [
        MkRule [E.sym "String" `E.app` [E.var "s"]] [E.sym "Html" `E.app` [E.var "a", E.var "s"]]
        , MkRule [] [E.sym "String" `E.app` [E.sym "var_n"]]
    ]
