module Meta.JavaRender where

import qualified Meta.File as F
import qualified Meta.Java as J
import qualified Meta.JavaSta as JS
import qualified Meta.JavaType as JT
import qualified Meta.WrapM as V

data RenOpt
    -- | Internal. Do not use. Use 'defRenOpt'.
    = MkRenOpt {
        roIndent :: Int
        , roCurInd :: Int
    } deriving (Read, Show)

defRenOpt :: RenOpt
defRenOpt = MkRenOpt {
        roIndent = 4
        , roCurInd = 0
    }

renderClassFile :: RenOpt -> J.Class -> F.File
renderClassFile ro cls =
    F.text path $ V.run V.defState { V.sIndSize = roIndent ro } $ renderClass cls
    where
        prefix = case pkg of
            "" -> ""
            _ -> map replace pkg ++ "/"
            where
                replace '.' = '/'
                replace x = x
        path = prefix ++ name ++ ".java"
        pkg = J.cPkg cls
        name = J.cName cls

renderClass :: J.Class -> V.Prog ()
renderClass cls = do
    V.atom $ "package " ++ pkg ++ ";"
    V.break >> V.break
    V.atom $ "public class " ++ name
    V.space >> pExtends >> V.space
    V.atom "{" >> V.break
    V.indented $ mapM_ renderMember members
    V.atom "}"
    where
        pkg = J.cPkg cls
        name = J.cName cls
        members = J.cMembers cls
        parent = J.cParentClass cls
        pExtends = case parent of
            Nothing -> V.nop
            Just sup -> V.atom $ "extends " ++ J.qualName sup

-- * Internal

renderMember :: J.Member -> V.Prog ()
renderMember mem = case mem of
    J.MField fld -> do
        V.atom $ "public " ++ JT.render (J.fType fld) ++ " " ++ J.fName fld ++ ";"
        V.break
    J.MMethod met ->
        renderMethod met
    J.MLineComment s -> do
        V.atom $ "//" ++ s
        V.break
    J.MBlockComment s -> do
        V.atom $ "/*" ++ s ++ "*/"
        V.break
    _ -> error $ "Meta.Java.renderJavaMember: not implemented: " ++ show mem

renderMethod :: J.Method -> V.Prog ()
renderMethod met = do
    mapM_ renderAnt ants
    V.atom $ "public " ++ JT.render (J.mRet met)
    V.space
    V.atom $ J.mName met ++ " ("
    renderParams pars
    V.atom ")" >> V.space >> pThrows >> V.space
    renderBlock body >> V.break
    where
        pThrows = case throws of
            [] -> V.nop
            _ -> do
                V.atom "throws" >> V.space
                commaSep $ map (V.atom . JT.render) throws
        throws = J.mThrows met
        pars = J.mParams met
        ants = J.mAnts met
        body = J.mBody met

renderAnt :: J.Ant -> V.Prog ()
renderAnt ant = do
    V.atom $ "@" ++ JT.render (J.aType ant)
    V.break

commaSep :: [V.Prog ()] -> V.Prog ()
commaSep [] = V.nop
commaSep [x] = x
commaSep (h : t) = h >> V.atom "," >> V.space >> commaSep t

renderParams :: [J.Param] -> V.Prog ()
renderParams pars = commaSep $ map renderParam pars

renderParam :: J.Param -> V.Prog ()
renderParam par = do
    V.atom $ JT.render (J.pType par) ++ " " ++ J.pName par

renderBlock :: [JS.Sta] -> V.Prog ()
renderBlock [] = V.atom "{}"
renderBlock stas = do
    V.atom "{" >> V.break
    V.indented $ mapM_ renderSta stas
    V.atom "}"

renderSta :: JS.Sta -> V.Prog ()
renderSta sta = case sta of
    JS.SDecl typ nam mInit -> do
        V.atom $ JT.render typ ++ " " ++ nam
        maybe V.nop (\ ini -> V.space >> V.atom "=" >> V.space >> renderExp ini) mInit
        end
    JS.SExp ex -> do
        renderExp ex >> end
    JS.SAsgn loc val -> do
        renderExp loc >> V.space >> V.atom "=" >> V.space >> renderExp val >> end
    JS.SIf con tru mFal -> do
        V.atom "if (" >> renderExp con >> V.atom ")" >> V.space
        renderBlock tru
        maybe V.nop (\ fal -> V.space >> V.atom "else" >> V.space >> renderBlock fal) mFal
        V.break
    JS.SRet mVal -> do
        V.atom "return" >> maybe V.nop renderExp mVal >> end
    _ -> error $ "Meta.JavaSta.renderSta: not implemented: " ++ show sta
    where
        end = V.atom ";" >> V.break

renderExp :: JS.Exp -> V.Prog ()
renderExp ex = case ex of
    JS.ENull -> V.atom "null"
    JS.EThis -> V.atom "this"
    JS.ESuper -> V.atom "super"
    JS.EEq a b -> V.atom "(" >> renderExp a >> V.atom ") == (" >> renderExp b >> V.atom ")"
    JS.ENe a b -> V.atom "(" >> renderExp a >> V.atom ") != (" >> renderExp b >> V.atom ")"
    JS.EName s -> V.atom s
    JS.EStr s -> V.atom $ "\"" ++ escStr s ++ "\""
    JS.EField tar nam -> V.atom "(" >> renderExp tar >> V.atom (")." ++ nam)
    JS.EFieldStatic tar nam -> V.atom $ tar ++ "." ++ nam
    JS.ECall tar nam args -> do
        V.atom "("
        renderExp tar
        V.atom $ ")." ++ nam ++ "("
        commaSep $ map renderExp args
        V.atom ")"
    JS.ECallStatic tar nam args -> do
        V.atom $ tar ++ "." ++ nam ++ "("
        commaSep $ map renderExp args
        V.atom ")"
    _ -> error $ "Meta.JavaSta.renderExp: not implemented: " ++ show ex
    where
        escStr = concatMap escChr
        escChr '"' = "\\\""
        escChr x = [x]
