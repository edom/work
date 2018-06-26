module Meta.JavaExp where

import qualified Data.Int as I

type Name = String

{- |
@s@ is statement type.

@t@ is type type.

@v@ is value type.
-}
data Exp s t v
    -- literal expressions
    -- = Pure v
    = ENull
    | EThis
    | ESuper
    | EChar Char
    | EInt32 I.Int32
    | EInt64 I.Int64
    | EDbl Double
    | EStr String
    -- arithmetic expressions
    | ENeg (Exp s t v)
    | EPlus (Exp s t v) (Exp s t v)
    | EMinus (Exp s t v) (Exp s t v)
    | EMul (Exp s t v) (Exp s t v)
    | EDiv (Exp s t v) (Exp s t v)
    | EMod (Exp s t v) (Exp s t v)
    -- bitwise expressions
    | EBitNot (Exp s t v) (Exp s t v)
    | EBitAnd (Exp s t v) (Exp s t v)
    | EBitOr (Exp s t v) (Exp s t v)
    | EBitXor (Exp s t v) (Exp s t v)
    | EShl (Exp s t v) (Exp s t v) -- ^ shift left
    | ESra (Exp s t v) (Exp s t v) -- ^ shift right arithmetic
    | ESrl (Exp s t v) (Exp s t v) -- ^ shift right logical
    -- logical expressions
    | ELogNot (Exp s t v) (Exp s t v)
    | ELogAnd (Exp s t v) (Exp s t v)
    | ELogOr (Exp s t v) (Exp s t v)
    | ELt (Exp s t v) (Exp s t v)
    | EGt (Exp s t v) (Exp s t v)
    | ELteq (Exp s t v) (Exp s t v)
    | EGteq (Exp s t v) (Exp s t v)
    | EEq (Exp s t v) (Exp s t v) -- ^ equality expression
    | ENe (Exp s t v) (Exp s t v) -- ^ non-equality expression
    | EName Name -- ^ name expression (local variable, parameter reference, class name, or field access) expression
    | EField (Exp s t v) Name -- ^ instance field access expression: target, field name
    | EFieldStatic Name Name -- ^ static field access expression: class name, field name
    | ECall (Exp s t v) Name [Exp s t v] -- ^ call expression: target, method name, arguments
    | ECallStatic Name Name [Exp s t v] -- ^ static call expression: class, method name, arguments
    | ENew t [Exp s t v] -- ^ instantiation expression
    | EIf (Exp s t v) (Exp s t v) (Exp s t v) -- ^ conditional expression: condition, true part, false part; the part not taken is not evaluated
    -- assignment
    | EAssign (Exp s t v) (Exp s t v) -- ^ assignment expression: lvalue (should be an 'EVar' or 'EField'), rvalue
    | EPreInc (Exp s t v)
    deriving (Read, Show)
