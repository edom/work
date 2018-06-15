module Meta.HsDat where

import qualified Data.List as L

import qualified Meta.HsCon as C
import qualified Meta.HsType as T

empty :: Dat
empty = MkDat "" [] []

setName :: String -> Dat -> Dat
setName s d = d { dName = s }

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
add (MkDat name1 cons1 ders1) (MkDat name2 cons2 ders2) = MkDat name cons ders
    where
        name = name1 ++ name2
        cons = cons1 ++ cons2
        ders = L.nub $ ders1 ++ ders2

-- * Internal

data Dat
    = MkDat {
        dName :: T.TypName
        , dCons :: [C.Con]
        , dDers :: [T.SymCls]
    } deriving (Read, Show)
