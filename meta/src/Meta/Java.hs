module Meta.Java where

import qualified Data.List as L

import qualified Meta.File as F
import qualified Meta.JavaSta as JS
import qualified Meta.JavaType as JT
import qualified Meta.Prop as P

-- * Type

type Name = String

-- | See also "Meta.JavaType".
typeOf :: Class -> JT.Type
typeOf cls = JT.ref $ qualName cls

-- * Class

data Class
    -- | Internal. Do not use. Use 'defClass'.
    = MkClass {
        cPkg :: String
        , cName :: String -- ^ Internal. Do not use. Use 'P.getName' and 'P.setName'.
        , cParentClass :: Maybe Class
        , cIfaceClasses :: [Class]
        , cMembers :: [Member]
        , cAnnots :: [Ant]
        , cComment :: String
        , cExternal :: Bool
    } deriving (Read, Show)

instance P.GetName Class String where getName = cName
instance P.SetName Class String where setName n c = c { cName = n }

-- ** Create class

defClass :: Class
defClass = MkClass {
        cPkg = ""
        , cName = ""
        , cParentClass = Nothing
        , cIfaceClasses = []
        , cMembers = []
        , cAnnots = []
        , cComment = ""
        , cExternal = False
    }

-- ** Derive subclass

extend :: Name -> Class -> Class
extend name sup = defClass {
        cPkg = cPkg sup
        , cName = name
        , cParentClass = Just sup
    }

-- ** Predefined classes

stringClass :: Class
stringClass = defClass {
        cPkg = "java.lang"
        , cName = "String"
        , cExternal = True
    }

-- * Annotation

data Ant
    -- | Internal. Do not use. Use 'annotFromType'
    = MkAnnot {
        aType :: JT.Type
    } deriving (Read, Show)

annotFromType :: JT.Type -> Ant
annotFromType = MkAnnot

-- ** Predefined annotations

a_Override :: Ant
a_Override = annotFromType $ JT.ref "java.lang.Override"

-- * Method parameter

data Param
    = MkParam {
        pType :: JT.Type
        , pName :: Name
        , pAnnots :: [Ant]
    } deriving (Read, Show)

mkParam :: JT.Type -> Name -> Param
mkParam typ nam = MkParam typ nam []

instance P.GetName Param String where getName = pName
instance P.SetName Param String where setName n p = p { pName = n }

-- * Type parameter

data TypeParam
    = MkTypeParam {
        tName :: String
    } deriving (Read, Show)

-- * Field

data Field
    -- | Internal. Do not use. Use 'mkField'.
    = MkField {
        fType :: JT.Type
        , fName :: Name
        , fAnts :: [Ant]
        , fComment :: String
    } deriving (Read, Show)

mkField :: JT.Type -> Name -> Field
mkField typ nam = defField { fType = typ, fName = nam }

-- * Method

data Method
    = MkMethod {
        mRet :: JT.Type
        , mName :: String
        , mTypeParams :: [TypeParam]
        , mParams :: [Param]
        , mThrows :: [JT.Type]
        , mBody :: [JS.Sta]
        , mAnts :: [Ant]
        , mComment :: String
    } deriving (Read, Show)

defMethod :: Method
defMethod = MkMethod {
        mRet = JT.Void
        , mName = ""
        , mTypeParams = []
        , mParams = []
        , mThrows = []
        , mBody = []
        , mAnts = []
        , mComment = ""
    }

-- ** Derive method

override :: Method -> Method
override met = met {
        mBody = []
        , mAnts = [a_Override]
    }

-- * Class member

data Member
    = MField Field
    | MMethod Method
    | MInit [JS.Sta]
    | MStaticInit [JS.Sta]
    | MLineComment String
    | MBlockComment String
    deriving (Read, Show)

-- * Expression

eCallStatic :: Class -> Name -> [JS.Exp] -> JS.Exp
eCallStatic tar nam arg = JS.eCallStatic (qualName tar) nam arg

eFieldStatic :: Class -> Name -> JS.Exp
eFieldStatic tar nam = JS.eFieldStatic (qualName tar) nam

-- * Render

data RenOpt
    -- | Internal. Do not use. Use 'defRenOpt'.
    = MkRenOpt {
        roIndent :: Int
    } deriving (Read, Show)

defRenOpt :: RenOpt
defRenOpt = MkRenOpt {
        roIndent = 4
    }

renderClass :: RenOpt -> Class -> F.File
renderClass ro cls = F.text path content
    where
        pkg = cPkg cls
        name = cName cls
        members = cMembers cls
        parent = cParentClass cls
        sExtends = case parent of
            Nothing -> ""
            Just sup -> " extends " ++ qualName sup
        prefix = case pkg of
            "" -> ""
            _ -> map replace pkg ++ "/"
            where
                replace '.' = '/'
                replace x = x
        path = prefix ++ name ++ ".java"
        content =
            "package " ++ pkg ++ ";\n\n"
            ++ "public class " ++ name ++ sExtends ++ " {\n"
            ++ (unlines $ map ("    " ++) $ map (renderMember ro) $ members)
            ++ "}\n"

renderMember :: RenOpt -> Member -> String
renderMember ro mem = case mem of
    MField fld -> "public " ++ JT.render (fType fld) ++ " " ++ fName fld ++ ";"
    MMethod met -> renderMethod ro met
    MLineComment s -> "//" ++ s;
    MBlockComment s -> "/*" ++ s ++ "*/";
    _ -> error $ "Meta.Java.renderJavaMember: not implemented: " ++ show mem

renderMethod :: RenOpt -> Method -> String
renderMethod ro met =
    sAnts
    ++ "public " ++ JT.render (mRet met) ++ " " ++ mName met ++ " (" ++ renderParams pars ++ ") "
    ++ sThrows
    ++ JS.renderBlock body
    where
        sAnts = unlines $ map renderAnt ants
        sThrows = case throws of
            [] -> ""
            _ -> "throws " ++ L.intercalate ", " (map JT.render throws)
        throws = mThrows met
        pars = mParams met
        ants = mAnts met
        body = mBody met

renderAnt :: Ant -> String
renderAnt ant = "@" ++ JT.render (aType ant)

renderParams :: [Param] -> String
renderParams pars = L.intercalate ", " $ map renderParam pars

renderParam :: Param -> String
renderParam par = JT.render (pType par) ++ " " ++ pName par

-- * Internal

-- | Internal. Do not use. Use 'mkField'.
defField :: Field
defField = MkField JT.Int32 "" [] ""

-- | Internal. Do not use.
qualName :: Class -> String
qualName cls = case pkg of
    "" -> name
    _ -> pkg ++ "." ++ name
    where
        pkg = cPkg cls
        name = cName cls
