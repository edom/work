module Meta.HsDat where

import qualified Data.List as L

import qualified Meta.HsCon as C
import qualified Meta.HsType as T

-- * ADT representation

-- ** Constructors

empty :: Dat
empty = MkDat "" [] [] []

-- ** Getters

getSyms :: Dat -> [T.Sym]
getSyms dat = concatMap C.getSyms (dCons dat) ++ dDers dat

-- ** Setters

setName :: String -> Dat -> Dat
setName s d = d { dName = s }

-- ** Manipulations

{- |
Compile-time union type.

Inputs:

@
data A = A1 | A2 | ... deriving (D1, D2, ...)
data B = B1 | B2 | ... deriving (E1, E2, ...)
@

Output:

@
data AB
    = A1 | A2 | ...
    | B1 | B2 | ...
    deriving (D1, D2, ..., E1, E2, ...)
@
-}
add :: Dat -> Dat -> Dat
add (MkDat name1 pars1 cons1 ders1) (MkDat name2 pars2 cons2 ders2) =
    if pars1 == pars2
        then MkDat name pars1 cons ders
        else error $ "Meta.HsDat.add: both ADTs must have the same type parameters: " ++ show pars1 ++ " != " ++ show pars2
    where
        name = name1 ++ name2
        cons = cons1 ++ cons2
        ders = L.nub $ ders1 ++ ders2

-- * ADT constructor representation

{- $
See also "Meta.HsCon".
-}

type Con = C.Con

-- ** Constructors

conRec :: T.ConName -> [(T.VarName, T.Type)] -> Con
conRec = C.CRec

-- * Internal

data Dat
    = MkDat {
        dName :: T.TypName
        , dPars :: [T.VarName] -- ^ type parameters
        , dCons :: [C.Con]
        , dDers :: [T.SymCls]
    } deriving (Read, Show)
