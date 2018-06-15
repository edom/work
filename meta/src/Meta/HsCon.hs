module Meta.HsCon where

import qualified Meta.HsType as T

getSyms :: Con -> [T.Sym]
getSyms con = case con of
    CCon _ ts -> concatMap T.getSyms ts
    CRec _ fs -> concatMap (\ (_, t) -> T.getSyms t) fs

-- * Internal

data Con
    = CCon T.ConName [T.Type]

    {- |
Record constructor.
    -}

    | CRec T.ConName [(T.VarName, T.Type)]

    deriving (Read, Show)
