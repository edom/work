module Meta.HsMod where

import qualified Data.List as L

import qualified Meta.HsCon as C
import qualified Meta.HsDat as D
import qualified Meta.HsExp as E
import qualified Meta.HsPat as P
import qualified Meta.HsType as T

-- * Module

mkModule :: T.ModName -> [Member] -> Module
mkModule modn mems = MkModule modn (fixImports mems)

-- * Members

-- | @impQual \"A.B.C\"@ represents @import qualified A.B.C@.
impQual :: T.ModName -> Member
impQual = Imp

mDat :: T.TypName -> [C.Con] -> [T.SymCls] -> Member
mDat name cons ders = MDat $ D.MkDat name cons ders

-- * Internal

data Module
    -- | Internal. Do not use. Use 'mkModule'.
    = MkModule {
        mName :: T.ModName
        , mMembers :: [Member]
    } deriving (Read, Show)

data Member
    -- | Internal. Do not use. Use 'impQual'. Represents @import qualified ModName@.
    = Imp T.ModName
    | Dec T.VarName T.Type -- ^ declaration -- varName :: type
    | Def T.VarName [P.Pat] E.Exp -- ^ definition -- varName pat1 ... patN = body

    {- |
ADT definition.

@
data TypName
    = pat1
    | pat2
    ...
    deriving (Cls1, Cls2, ...)
@
    -}

    | MDat D.Dat

    {- |
Class definition.

@
class ClsName pat1 pat2 ... where
    member1
    member2
    ...
@
    -}

    | Cls T.ClsName [P.Pat] [Member]

    {- |
Instance definition.

@
instance ClsName type1 type2 ... where
    member1
    member2
    ...
@
    -}

    | Ins T.ClsName [T.Type] [Member]

    deriving (Read, Show)

memGetSyms :: Member -> [T.Sym]
memGetSyms mem = case mem of
    Imp _ -> []
    Dec _ t -> T.getSyms t
    Def _ pats ex -> concatMap P.getSyms pats ++ E.getSyms ex
    MDat (D.MkDat _ cons ders) -> concatMap C.getSyms cons ++ ders
    Cls _ pats mems -> concatMap P.getSyms pats ++ concatMap memGetSyms mems
    Ins _ typs mems -> concatMap T.getSyms typs ++ concatMap memGetSyms mems

-- nub is O(n^2).
computeImports :: [Member] -> [Member]
computeImports mems = map impQual $ L.sort $ L.nub $ map T.sMod $ concatMap (\ m -> memGetSyms m) mems

fixImports :: [Member] -> [Member]
fixImports mems = computeImports mems ++ mems
