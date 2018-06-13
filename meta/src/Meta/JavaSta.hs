module Meta.JavaSta where

import qualified Data.Int as I
import qualified Data.List as L

import qualified Meta.JavaType as JT

-- * Statement

sIf :: Exp -> [Sta] -> Sta
sIf con tru = SIf con tru Nothing

sIfElse :: Exp -> [Sta] -> [Sta] -> Sta
sIfElse con tru fal = SIf con tru (Just fal)

sRet :: Exp -> Sta
sRet r = SRet (Just r)

sRetVoid :: Sta
sRetVoid = SRet Nothing

sCall :: Exp -> Name -> [Exp] -> Sta
sCall tar nam arg = SExp $ eCall tar nam arg

sDecl :: JT.Type -> Name -> Sta
sDecl typ nam = SDecl typ nam Nothing

sDef :: JT.Type -> Name -> Exp -> Sta
sDef typ nam ini = SDecl typ nam (Just ini)

sAsgn :: Exp -> Exp -> Sta
sAsgn loc val = SAsgn loc val

renderBlock :: [Sta] -> String
renderBlock stas =
    case stas of
        [] -> "{}"
        _ -> "{\n"
            ++ unlines (map renderSta stas)
            ++ "}\n"

renderSta :: Sta -> String
renderSta sta = case sta of
    SDecl typ nam mini -> JT.render typ ++ " " ++ nam ++ maybe "" (\ ini -> " = " ++ renderExp ini) mini ++ ";"
    SExp exp -> renderExp exp ++ ";"
    SAsgn loc val -> renderExp loc ++ " = " ++ renderExp val ++ ";"
    SIf con tru mfal ->
        "if (" ++ renderExp con ++ ") " ++ renderBlock tru
        ++ maybe "" (\ fal -> "else " ++ renderBlock fal) mfal
    SRet mval -> "return" ++ maybe "" renderExp mval ++ ";"
    _ -> error $ "Meta.JavaSta.renderSta: not implemented: " ++ show sta

{- |
Do not use these data type constructors directly. Use constructor functions.
-}
data Sta
    = SExp Exp
    | SRet (Maybe Exp)
    | SThrow Exp
    | SBlock [Sta]
    | SIf Exp [Sta] (Maybe [Sta]) -- ^ conditional statement: condition, true part, false part
    | SSwitch Exp [(Exp, [Sta])] -- ^ switch expression
    | SBreak
    | SDecl JT.Type Name (Maybe Exp)
    | SAsgn Exp Exp -- ^ assignment statement: lvalue (should be an 'EVar' or 'EField'), rvalue
    deriving (Read, Show)

data Case
    = CCase Exp [Sta]
    | CDefault [Sta]
    deriving (Read, Show)

-- * Expression

type Name = String

eStr :: String -> Exp
eStr = EStr

eField :: Exp -> String -> Exp
eField = EField

renderExp :: Exp -> String
renderExp ex = case ex of
    ENull -> "null"
    EThis -> "this"
    ESuper -> "super"
    EEq a b -> "(" ++ renderExp a ++ ") == (" ++ renderExp b ++ ")"
    ENe a b -> "(" ++ renderExp a ++ ") != (" ++ renderExp b ++ ")"
    EName s -> s
    EStr s -> "\"" ++ escStr s ++ "\""
    EField tar nam -> "(" ++ renderExp tar ++ ")." ++ nam
    EFieldStatic tar nam -> tar ++ "." ++ nam
    ECall tar nam args -> "(" ++ renderExp tar ++ ")." ++ nam ++ "(" ++ L.intercalate ", " (map renderExp args) ++ ")"
    ECallStatic tar nam args -> tar ++ "." ++ nam ++ "(" ++ L.intercalate ", " (map renderExp args) ++ ")"
    _ -> error $ "Meta.JavaSta.renderExp: not implemented: " ++ show ex
    where
        escStr = concatMap escChr
        escChr '"' = "\\\""
        escChr x = [x]

data Exp
    -- literal expressions
    = ENull
    | EThis
    | ESuper
    | EChar Char
    | EInt32 I.Int32
    | EInt64 I.Int64
    | EDbl Double
    | EStr String
    -- arithmetic expressions
    | ENeg Exp
    | EPlus Exp Exp
    | EMinus Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EMod Exp Exp
    -- bitwise expressions
    | EBitNot Exp Exp
    | EBitAnd Exp Exp
    | EBitOr Exp Exp
    | EBitXor Exp Exp
    | EShl Exp Exp -- ^ shift left
    | ESra Exp Exp -- ^ shift right arithmetic
    | ESrl Exp Exp -- ^ shift right logical
    -- logical expressions
    | ELogNot Exp Exp
    | ELogAnd Exp Exp
    | ELogOr Exp Exp
    | EEq Exp Exp -- ^ equality expression
    | ENe Exp Exp -- ^ non-equality expression
    | EName Name -- ^ name expression (local variable, parameter reference, class name, or field access) expression
    | EField Exp Name -- ^ instance field access expression: target, field name
    | EFieldStatic Name Name -- ^ static field access expression: class name, field name
    | ECall Exp Name [Exp] -- ^ call expression: target, method name, arguments
    | ECallStatic Name Name [Exp] -- ^ static call expression: class, method name, arguments
    | ENew JT.Type [Exp] -- ^ instantiation expression
    | EIf Exp Exp Exp -- ^ conditional expression: condition, true part, false part; the part not taken is not evaluated
    deriving (Read, Show)

eName :: Name -> Exp
eName = EName

eCall :: Exp -> Name -> [Exp] -> Exp
eCall tar nam arg = ECall tar nam arg

eCallStatic :: Name -> Name -> [Exp] -> Exp
eCallStatic tar nam arg = ECallStatic tar nam arg

eFieldStatic :: Name -> Name -> Exp
eFieldStatic tar nam = EFieldStatic tar nam

-- * Convenience expression constructors

-- | @eEquals a b@ produces @java.util.Objects.equals(a, b)@.
eEquals :: Exp -> Exp -> Exp
eEquals a b = eCallStatic "java.util.Objects" "equals" [a, b]

-- | @eIsNull a@ produces @a == null@.
eIsNull :: Exp -> Exp
eIsNull a = EEq a ENull
