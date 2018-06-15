module Meta.HsPat where

import qualified Meta.HsType as T

-- * Symbols

getSyms :: Pat -> [T.Sym]
getSyms pat = case pat of
    PVar _ -> []
    PCon c pats -> c : concatMap getSyms pats
    PAny -> []

-- * Internal

data Pat
    = PVar T.VarName -- ^ var
    | PCon T.SymCon [Pat] -- ^ Con pat1 pat2 ...
    | PAny -- ^ _
    deriving (Eq, Read, Show)
