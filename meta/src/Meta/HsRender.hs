module Meta.HsRender where

import qualified Meta.File as F
import qualified Meta.HsCon as C
import qualified Meta.HsDat as D
import qualified Meta.HsMod as M
import qualified Meta.HsType as T
import qualified Meta.WrapM as W

renderType :: T.Type -> W.Prog ()
renderType ty = case ty of
    T.Val sym -> W.atom $ T.sQualName sym
    T.Var n -> W.atom n
    T.App a b -> do
        W.atom "(" >> renderType a >> W.atom ")"
        W.space
        W.atom "(" >> renderType b >> W.atom ")"
    T.Lam a b -> do
        W.atom "forall" >> W.space >> W.atom a >> W.atom "."
        W.space
        renderType b
    T.Arr a b -> do
        renderType a >> W.space >> W.atom "->" >> W.space >> renderType b

renderModule :: M.Module -> W.Prog ()
renderModule mdl = do
    W.atom ("module " ++ M.mName mdl ++ " where") >> W.break >> W.break
    renderMembers $ M.mMembers mdl

renderModuleFile :: M.Module -> F.File
renderModuleFile mdl = F.text path content
    where
        path = map replace modName ++ ".hs"
            where
                replace '.' = '/'
                replace x = x
        content = W.run W.defState $ renderModule mdl
        modName = M.mName mdl

renderMembers :: [M.Member] -> W.Prog ()
renderMembers ms = case ms of
    [] -> W.nop
    h@(M.Imp _) : t@(M.Imp _ : _) -> renderMember h >> renderMembers t
    h@(M.Imp _) : t -> renderMember h >> W.break >> renderMembers t -- extra break after import list
    h : t -> renderMember h >> renderMembers t

renderMember :: M.Member -> W.Prog ()
renderMember mem = case mem of
    M.Imp modName -> do
        W.atom ("import qualified " ++ modName) >> W.break
    M.Dec nam typ -> do
        W.atom nam >> W.space >> W.atom "::" >> W.space >> renderType typ >> W.break
    M.MDat (D.MkDat tyNam pars cons ders) -> do
        W.atom ("data " ++ tyNam) >> W.sepBy W.space (map W.atom pars) >> W.break
        W.indented $ do
            sequence_ $ zipWith renderCon begsym cons
            W.atom "deriving ("
            W.commaSep $ map (W.atom . T.sQualName) $ ders
            W.atom ")" >> W.break
            W.break
    M.LineComment s -> W.atom ("--" ++ s) >> W.break
    M.BlockComment s -> W.atom $ "{-" ++ s ++ "-}"
    _ -> error $ "Meta.HsRender.renderMember: " ++ show mem
    where
        begsym = "= " : repeat "| "

renderCon :: String -> C.Con -> W.Prog ()
renderCon lef con = case con of
    C.CRec conName flds -> do
        W.atom (lef ++ conName ++ " {") >> W.break
        W.indented $ sequence_ $ zipWith renderField begsym flds
        W.atom "}"
        W.break
    _ -> error $ "Meta.HsRender.renderCon: " ++ show con
    where
        begsym = "" : repeat ", "

renderField :: String -> (T.VarName, T.Type) -> W.Prog ()
renderField lef (nam, typ) = do
    W.atom (lef ++ nam) >> W.space >> W.atom "::" >> W.space >> renderType typ >> W.break
