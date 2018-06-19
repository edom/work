{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
* Naming conventions:

    * s\_: statement constructors

    * e\_: expression constructors

    * t\_: type constructors
-}
module Meta.Java where

import qualified Data.Int as I
import qualified Meta.JavaSta as JS
import qualified Meta.JavaType as JT
import qualified Meta.Prop as P

-- * Type

type Name = String

type Type = JT.Type

-- | See also "Meta.JavaType".
typeOf :: Class -> Type
typeOf cls = JT.ref $ qualName cls

type Qual_name = String

t_ref :: Qual_name ->Type
t_ref = JT.ref

t_byte :: Type
t_byte = JT.primInt8

t_int :: Type
t_int = JT.primInt32

t_SqlException :: Type
t_SqlException = JT.ref "java.sql.SQLException"

t_ServletException :: Type
t_ServletException = JT.ref "javax.servlet.ServletException"

t_InputStream :: Type
t_InputStream = JT.ref "java.io.InputStream"

t_IOException :: Type
t_IOException = JT.ref "java.io.IOException"

t_URL :: Type
t_URL = JT.ref "java.net.URL"

t_array_of :: Type -> Type
t_array_of t = JT.Array t

-- * Class

type Package_name = String

data Class
    -- | Internal. Do not use. Use 'defClass'.
    = MkClass {
        cPkg :: Package_name
        , cName :: String -- ^ Internal. Do not use. Use 'P.getName' and 'P.setName'.
        , cParentClass :: Maybe Class
        , cIfaceClasses :: [Class]
        , cMembers :: [Member]
        , cAnnots :: [Ant]
        , cComment :: String
        , cExternal :: Bool
    } deriving (Read, Show)

c_set_name :: String -> Class -> Class
c_set_name n c = c { cName = n }

class C_add a where c_add :: a -> Class -> Class
instance C_add Field where c_add = c_add_member . MField
instance C_add Method where c_add = c_add_member . MMethod
instance C_add Member where c_add = c_add_member
instance C_add Ant where c_add = c_add_ant
instance C_add a => C_add [a] where
    c_add [] = id
    c_add (h : t) = c_add t . c_add h

c_add_ant :: Ant -> Class -> Class
c_add_ant ant cls = cls { cAnnots = cAnnots cls ++ [ant] }

c_add_field :: Type -> Name -> Class -> Class
c_add_field typ nam = c_add_member $ MField $ mkField typ nam

c_add_member :: Member -> Class -> Class
c_add_member mem cls = cls { cMembers = cMembers cls ++ [mem] }

c_add_line_comment :: String -> Class -> Class
c_add_line_comment str = c_add_member $ MLineComment str

set_pkg :: Package_name -> Class -> Class
set_pkg p c = c { cPkg = p }

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
        aType :: Type
        , aParams :: [Exp]
    } deriving (Read, Show)

annotFromType :: Type -> Ant
annotFromType typ = MkAnnot {
        aType = typ
        , aParams = []
    }

-- ** Predefined annotations

a_Override :: Ant
a_Override = annotFromType $ JT.ref "java.lang.Override"

a_Inject :: Ant
a_Inject = annotFromType $ JT.ref "javax.inject.Inject"

a_Singleton :: Ant
a_Singleton = annotFromType $ JT.ref "javax.inject.Singleton"

a_Named :: String -> Ant
a_Named nam = (annotFromType $ JT.ref "javax.inject.Named") {
        aParams = [JS.eStr nam]
    }

-- * Method parameter

data Param
    = MkParam {
        pType :: Type
        , pName :: Name
        , pAnnots :: [Ant]
    } deriving (Read, Show)

mkParam :: Type -> Name -> Param
mkParam typ nam = MkParam typ nam []

instance P.GetName Param String where getName = pName
instance P.SetName Param String where setName n p = p { pName = n }

param_add_ants :: [Ant] -> Param -> Param
param_add_ants ants par = par { pAnnots = pAnnots par ++ ants }

-- * Type parameter

data TypeParam
    = MkTypeParam {
        tName :: String
    } deriving (Read, Show)

-- * Field

data Field
    -- | Internal. Do not use. Use 'mkField'.
    = MkField {
        fType :: Type
        , fName :: Name
        , fAnts :: [Ant]
        , fComment :: String
        , fFinal :: Bool
        , fAccess :: Access
    } deriving (Read, Show)

-- | Internal. Do not use. Use 'mkField'.
defField :: Field
defField = MkField {
        fType = JT.Int32
        , fName = ""
        , fAnts = []
        , fComment = ""
        , fFinal = False
        , fAccess = Package
    }

data Access
    = Private
    | Package
    | Protected
    | Public
    deriving (Read, Show)

mkField :: Type -> Name -> Field
mkField typ nam = defField { fType = typ, fName = nam }

-- * Method

data Method
    = MkMethod {
        mRet :: Type
        , mName :: String
        , mTypeParams :: [TypeParam]
        , mParams :: [Param]
        , mThrows :: [Type]
        , mBody :: [JS.Sta]
        , mAnts :: [Ant]
        , mComment :: String
        , _is_ctor :: Bool
        , mFinal :: Bool
        , mAccess :: Access
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
        , _is_ctor = False
        , mFinal = False
        , mAccess = Package
    }

ctor :: Method
ctor = defMethod { _is_ctor = True }

class M_add a where m_add :: a -> Method -> Method
instance M_add Ant where m_add = m_add_ant
instance M_add Param where m_add = m_add_param
instance M_add a => M_add [a] where
    m_add [] = id
    m_add (h : t) = m_add t . m_add h

m_add_param :: Param -> Method -> Method
m_add_param p m = m { mParams = mParams m ++ [p] }

m_add_ant :: Ant -> Method -> Method
m_add_ant a m = m { mAnts = mAnts m ++ [a] }

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

type Exp = JS.Exp

e_call_static :: Qual_name -> Method_name -> [Exp] -> Exp
e_call_static tar nam arg = JS.ECallStatic tar nam arg

-- | Deprecated.
eCallStatic :: Class -> Name -> [JS.Exp] -> JS.Exp
eCallStatic tar nam arg = JS.eCallStatic (qualName tar) nam arg

eFieldStatic :: Class -> Name -> JS.Exp
eFieldStatic tar nam = JS.eFieldStatic (qualName tar) nam

e_field :: Exp -> Name -> Exp
e_field = JS.eField

e_this :: Exp
e_this = JS.EThis

e_name :: Name -> Exp
e_name = JS.eName

e_int :: I.Int32 -> Exp
e_int = JS.EInt32

e_str :: String -> Exp
e_str = JS.eStr

e_new :: Type -> [Exp] -> Exp
e_new = JS.ENew

e_null :: Exp
e_null = JS.ENull

e_is_null :: Exp -> Exp
e_is_null a = JS.EEq a JS.ENull

-- | @[ e_toString e ] => java.util.Objects.toString( [ e ] )@
e_toString :: Exp -> Exp
e_toString e = JS.eCallStatic "java.util.Objects" "toString" [e]

type Int32 = I.Int32

class E_int32 a where e_int32 :: a -> Exp
instance E_int32 Int where e_int32 = JS.EInt32 . fromIntegral
instance E_int32 I.Int32 where e_int32 = JS.EInt32

e_int32i :: Int -> Exp
e_int32i = e_int32

type Method_name = String

e_call :: Exp -> Method_name -> [Exp] -> Exp
e_call = JS.eCall

e_str_empty :: Exp
e_str_empty = JS.eStr ""

e_plus :: Exp -> Exp -> Exp
e_plus = JS.EPlus

e_lt :: Exp -> Exp -> Exp
e_lt = JS.ELt

e_lteq :: Exp -> Exp -> Exp
e_lteq = JS.ELteq

e_preincrement :: Exp -> Exp
e_preincrement = JS.EPreInc

e_assign :: Exp -> Exp -> Exp
e_assign = JS.EAssign

-- * Statement

type Sta = JS.Sta

s_block :: [Sta] -> Sta
s_block = JS.SBlock

s_def :: Type -> Name -> Exp -> Sta
s_def = JS.sDef

s_assign :: Exp -> Exp -> Sta
s_assign = JS.sAsgn

s_while :: Exp -> [Sta] -> Sta
s_while = JS.SWhile

s_throw :: Exp -> Sta
s_throw = JS.SThrow

s_if :: Exp -> [Sta] -> Sta
s_if = JS.sIf

s_call :: Exp -> Method_name -> [Exp] -> Sta
s_call target method args = s_exp (e_call target method args)

s_exp :: Exp -> Sta
s_exp = JS.SExp

type Catch = JS.Catch

s_try :: [Sta] -> Sta
s_try body = JS.STry body [] Nothing

catch :: Type -> Name -> [Sta] -> Sta -> Sta
catch ty na bo sta = case sta of
    JS.STry t cs f -> JS.STry t (cs ++ c) f
    JS.SWith defs body cs mf -> JS.SWith defs body (cs ++ c) mf
    _ -> error $ "Meta.Java.catch: " ++ show sta
    where
        c = [JS.MkCatch ty na bo]

finally ::[Sta] -> Sta -> Sta
finally f sta = case sta of
    JS.STry t cs _ -> JS.STry t cs (Just f)
    JS.SWith defs body cs _ -> JS.SWith defs body cs (Just f)
    _ -> error $ "Meta.Java.finally: " ++ show sta

-- | Try-with-resources.
s_with :: [Sta] -> [Sta] -> Sta
s_with defs body = JS.SWith defs body [] Nothing

s_for :: Sta -> Exp -> Exp -> [Sta] -> Sta
s_for = JS.SFor

qualName :: Class -> String
qualName cls = case pkg of
    "" -> name
    _ -> pkg ++ "." ++ name
    where
        pkg = cPkg cls
        name = cName cls
