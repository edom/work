module Meta.HsExp where

import qualified Meta.HsType as T
import qualified Meta.HsPat as P

-- * Symbols

getSyms :: Exp -> [T.Sym]
getSyms ex = case ex of
    ECha _ -> []
    EInt _ -> []
    EStr _ -> []
    EVar _ -> []
    ELam _ e -> getSyms e
    EApp a b -> getSyms a ++ getSyms b
    ECase big cs -> getSyms big ++ concatMap (\ (_, e) -> getSyms e) cs

-- * Metaprograms

mergeCase :: Exp -> Exp -> Exp
mergeCase (ECase exp1 cases1) (ECase exp2 cases2) =
    if exp1 == exp2
        then ECase exp1 (cases1 ++ cases2)
        else error $ "Meta.Hs.mergeCase: exp1 != exp2: (" ++ show exp1 ++ ") (" ++ show exp2 ++ ")"
mergeCase a b =
    error $ "Meta.Hs.mergeCase: only works with ECase: (" ++ show a ++ ") (" ++ show b ++ ")"

-- * Internal

-- | Lambda calculus at universe 0.
data Exp
    {- values -}
    = ECha Char
    | EInt Integer
    | EStr String
    {- lambda calculus -}
    | EVar T.VarName
    | ELam T.VarName Exp
    | EApp Exp Exp
    {- something else -}
    | ECase Exp [(P.Pat, Exp)]
    deriving (Eq, Read, Show)
