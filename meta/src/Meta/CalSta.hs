{-# LANGUAGE RecordWildCards #-}

module Meta.CalSta where

import Prelude hiding (exp, seq)

import qualified Meta.CalExp as E
import qualified Meta.CalVal as V

type Name = String

type Val = V.Val

data Exp a
    = Pure a
    | EExp (E.Exp (Exp a))
    | Var Name
    deriving (Read, Show)

int :: Int -> Exp Val
int i = Pure (V.Int i)

plus :: Exp Val -> Exp Val -> Exp Val
plus a b = EExp (E.Plus a b)

infixl 4 `plus`

eval :: [Binding Val] -> Exp Val -> E.Exp Val
eval _ (Pure val) = E.Pure val
eval binds (EExp exp) = fmap (E.eval . eval binds) exp
eval [] (Var name) = E.Pure $ V.Error ["Meta.CalSta.eval: undefined variable: " ++ name]
eval ((n,v):_) (Var name) | n == name = E.Pure v
eval (_:t) (Var name) = eval t (Var name)

data Sta a
    = SExp (Exp a)
    | Assign Name (Exp a)
    | Seq (Sta a) (Sta a)
    | Halt
    deriving (Read, Show)

infix 1 `Assign`
infixr 0 `Seq`

type Binding a = (Name, a)

data State
    = MkState {
        sHalted :: Bool
        , sBindings :: [Binding Val]
        , sErrors :: [String]
    } deriving (Read, Show)

sInit :: State
sInit = MkState {
        sHalted = False
        , sBindings = []
        , sErrors = []
    }

set :: Name -> a -> [Binding a] -> [Binding a]
set name val [] = [(name, val)]
set name val ((n,_):t) | name == n = (name,val) : t
set name val (h:t) = h : set name val t

execute :: Sta Val -> State
execute sta = exec sta sInit

exec :: Sta Val -> State -> State
exec sta state@MkState{..} =
    if sHalted
        then state
        else case sta of
            SExp _ -> state
            Assign name exp ->
                let val = E.eval $ eval sBindings exp
                in case val of
                    V.Error msgs -> state {
                            sHalted = True
                            , sErrors = sErrors ++ msgs
                        }
                    _ -> state { sBindings = set name val sBindings }
            Seq a b -> exec b . exec a $ state
            Halt -> state { sHalted = True }

ex0 :: Sta Val
ex0 =
    "x" `Assign` int 10
    `Seq` "z" `Assign` Var "x" `plus` Var "y"
    `Seq` "y" `Assign` int 11
