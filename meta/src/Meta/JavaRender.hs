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

render_class_file :: J.Class -> F.File
render_class_file = renderClassFileWith defRenOpt

renderClassFileWith :: RenOpt -> J.Class -> F.File
renderClassFileWith ro cls =
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
    mapM_ renderAnt $ J.cAnnots cls
    V.atom $ "public class " ++ name
    V.space >> pExtends >> V.space
    V.atom "{" >> V.break
    V.indented $ renderMembers cls members
    V.atom "}"
    where
        pkg = J.cPkg cls
        name = J.cName cls
        members = J.cMembers cls
        parent = J.cParentClass cls
        pExtends = case parent of
            Nothing -> V.nop
            Just sup -> V.atom $ "extends " ++ J.qualName sup

renderFinal :: Bool -> V.Prog ()
renderFinal fin = case fin of
    False -> V.nop
    True -> V.atom "final" >> V.space

-- * Internal

renderMembers :: J.Class -> [J.Member] -> V.Prog ()
renderMembers cls members = case members of
    [] -> V.nop
    h@(J.MField _) : t@(J.MMethod _ : _) -> do
        renderMember cls h >> V.break >> renderMembers cls t
    h@(J.MMethod _) : t@(J.MMethod _ : _) -> do
        renderMember cls h >> V.break >> renderMembers cls t
    h : t -> renderMember cls h >> renderMembers cls t
renderMember :: J.Class -> J.Member -> V.Prog ()
renderMember cls mem = case mem of
    J.MField fld -> do
        let acs = J.fAccess fld
        mapM_ renderAnt $ J.fAnts fld
        renderAccess acs
        renderFinal $ J.fFinal fld
        V.atom $ JT.render (J.fType fld)
        V.space
        V.atom $ J.fName fld ++ ";"
        V.break
    J.MMethod met ->
        renderMethod cls met
    J.MLineComment s -> do
        V.atom $ "//" ++ s
        V.break
    J.MBlockComment s -> do
        V.atom $ "/*" ++ s ++ "*/"
        V.break
    _ -> error $ "Meta.Java.renderJavaMember: not implemented: " ++ show mem

renderAccess :: J.Access -> V.Prog ()
renderAccess acs = case acs of
    J.Private -> V.atom "private" >> V.space
    J.Package -> V.nop
    J.Protected -> V.atom "protected" >> V.space
    J.Public -> V.atom "public" >> V.space

renderMethod :: J.Class -> J.Method -> V.Prog ()
renderMethod cls met = do
    let acs = J.mAccess met
    mapM_ renderAnt ants
    renderAccess acs
    renderFinal $ J.mFinal met
    if is_ctor
        then do
            V.atom $ J.cName cls ++ " ("
            V.break
            V.indented $ render_params_lines pars
        else do
            V.atom $ JT.render (J.mRet met)
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
                V.commaSep $ map (V.atom . JT.render) throws
        throws = J.mThrows met
        pars = J.mParams met
        ants = J.mAnts met
        body = J.mBody met
        is_ctor = J._is_ctor met

renderAnt :: J.Ant -> V.Prog ()
renderAnt ant = do
    render_ant_bare ant >> V.break

render_ant_bare :: J.Ant -> V.Prog ()
render_ant_bare ant = do
    V.atom $ "@" ++ JT.render (J.aType ant)
    if has_params
        then do
            V.atom "("
            V.commaSep $ map renderExp params
            V.atom ")"
        else V.nop
    where
        params = J.aParams ant
        has_params = not $ null params

renderParams :: [J.Param] -> V.Prog ()
renderParams pars = V.commaSep $ map renderParam pars

render_params_lines :: [J.Param] -> V.Prog ()
render_params_lines pars = case pars of
    [] -> V.nop
    h : t -> renderParam h >> V.break >> render_params_lines t

renderParam :: J.Param -> V.Prog ()
renderParam par = do
    flip mapM_ ants $ \ ant -> render_ant_bare ant >> V.space
    V.atom $ JT.render (J.pType par) ++ " " ++ J.pName par
    where
        ants = J.pAnnots par

renderBlock :: [JS.Sta] -> V.Prog ()
renderBlock [] = V.atom "{}"
renderBlock stas = do
    V.atom "{" >> V.break
    V.indented $ mapM_ renderSta stas
    V.atom "}"

try_render_sta_inline :: JS.Sta -> Maybe (V.Prog ())
try_render_sta_inline sta = case sta of
    JS.SDecl typ nam mInit -> Just $ do
        V.atom $ JT.render typ ++ " " ++ nam
        maybe V.nop (\ ini -> V.space >> V.atom "=" >> V.space >> renderExp ini) mInit
        end
    JS.SExp ex -> Just $ do
        renderExp ex >> end
    _ -> Nothing
    where
        end = V.atom ";"

renderSta :: JS.Sta -> V.Prog ()
renderSta sta | Just prog <- try_render_sta_inline sta = prog >> V.break
renderSta sta = case sta of
    JS.SBlock stas -> do
        V.atom "{"
        V.break
        V.indented $ mapM_ renderSta stas
        V.atom "}"
        V.break
    JS.SWith defs body catches m_finally -> do
        V.atom "try ("
        V.indented $ mapM_ renderSta defs
        V.atom ") {"
        V.break
        V.indented $ mapM_ renderSta body
        V.atom "}"
        V.break
        render_catches catches
        render_m_finally m_finally
    JS.STry body catches m_finally -> do
        V.atom "try {"
        V.indented $ mapM_ renderSta body
        V.atom "}"
        V.break
        render_catches catches
        render_m_finally m_finally
    JS.SWhile cond body -> do
        V.atom "while ("
        V.indented $ renderExp cond
        V.atom ") {"
        V.break
        V.indented $ mapM_ renderSta body
        V.atom "}"
        V.break
    JS.SFor sinit econd enext body -> do
        V.atom "for ("
        V.indented $ do
            maybe (renderSta sinit) id $ try_render_sta_inline sinit
            V.space
            renderExp econd
            V.atom ";"
            V.space
            renderExp enext
        V.atom ") {"
        V.break
        V.indented $ mapM_ renderSta body
        V.atom "}"
        V.break
    JS.SThrow e -> do
        V.atom "throw"
        V.space
        renderExp e
        end
    JS.SIf con tru mFal -> do
        V.atom "if (" >> renderExp con >> V.atom ")" >> V.space
        renderBlock tru
        maybe V.nop (\ fal -> V.space >> V.atom "else" >> V.space >> renderBlock fal) mFal
        V.break
    JS.SRet mVal -> do
        V.atom "return" >> maybe V.nop renderExp mVal >> end
    _ -> error $ "Meta.JavaRender.renderSta: not implemented: " ++ show sta
    where
        end = V.atom ";" >> V.break

render_catches :: [JS.Catch] -> V.Prog ()
render_catches = mapM_ render_catch

render_catch :: JS.Catch -> V.Prog ()
render_catch c = do
    V.atom "catch ("
    V.atom $ JT.render (JS._catch_type c)
    V.space
    V.atom (JS._catch_name c)
    V.atom ") {"
    V.break
    V.indented $ mapM_ renderSta (JS._catch_body c)
    V.atom "}"
    V.break

render_m_finally :: JS.Finally -> V.Prog ()
render_m_finally m = case m of
    Nothing -> V.nop
    Just body -> do
        V.atom "finally {"
        V.indented $ mapM_ renderSta body
        V.atom "}"
        V.break

renderExp :: JS.Exp -> V.Prog ()
renderExp ex = case ex of
    JS.ENull -> V.atom "null"
    JS.EThis -> V.atom "this"
    JS.ESuper -> V.atom "super"
    JS.EEq a b -> V.atom "(" >> renderExp a >> V.atom ") == (" >> renderExp b >> V.atom ")"
    JS.ENe a b -> V.atom "(" >> renderExp a >> V.atom ") != (" >> renderExp b >> V.atom ")"
    JS.ELteq a b -> parene a >> satom "<=" >> parene b
    JS.EName s -> V.atom s
    JS.EStr s -> V.atom $ "\"" ++ escStr s ++ "\""
    JS.EInt32 n -> V.atom $ show n
    JS.EInt64 n -> V.atom $ show n
    JS.EField tar nam -> do
        -- Parenthesizing "this" affects constructors of classes having final fields.
        -- If "this" is parenthesized, javac complains with "cannot assign a value to final variable".
        let no_par = renderExp tar >> V.atom ("." ++ nam)
            with_par = V.atom "(" >> renderExp tar >> V.atom (")." ++ nam)
        case tar of
            JS.EThis -> no_par
            JS.ESuper -> no_par
            _ -> with_par
    JS.EFieldStatic tar nam -> V.atom $ tar ++ "." ++ nam
    JS.ECall tar nam args -> do
        V.atom "("
        renderExp tar
        V.atom $ ")." ++ nam ++ "("
        V.commaSep $ map renderExp args
        V.atom ")"
    JS.ECallStatic tar nam args -> do
        V.atom $ tar ++ "." ++ nam ++ "("
        V.commaSep $ map renderExp args
        V.atom ")"
    JS.EPlus a b -> parene a >> satom "+" >> parene b
    JS.EAssign a b -> parene a >> satom "=" >> parene b
    JS.EPreInc a -> V.atom "++" >> parene a
    JS.ENew t ps -> do
        V.atom "new"
        V.space
        V.atom $ JT.render t
        V.atom "("
        V.commaSep $ map renderExp ps
        V.atom ")"
    _ -> error $ "Meta.JavaRender.renderExp: not implemented: " ++ show ex
    where
        escStr = concatMap escChr
        escChr '"' = "\\\""
        escChr x = [x]
        parene e = V.atom "(" >> renderExp e >> V.atom ")"
        satom x = V.space >> V.atom x >> V.space
