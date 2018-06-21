{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Meta.Cal where

import Prelude hiding (exp)

data Exp a
    = EPure a
    | EPlus a a
    | EEq a a
    deriving (Read, Show)

instance Functor Exp where
    fmap f (EPure a) = EPure (f a)
    fmap f (EPlus a b) = EPlus (f a) (f b)
    fmap f (EEq a b) = EEq (f a) (f b)

eval_val :: Exp Val -> Val
eval_val exp = case exp of
    EPure a -> a
    EPlus a b -> val_plus a b
    EEq a b -> val_eq a b

render_exp :: (a -> String) -> Exp a -> String
render_exp prin exp = case exp of
    EPure a -> prin a
    EPlus a b -> "(" ++ prin a ++ " + " ++ prin b ++ ")"
    EEq a b -> "(" ++ prin a ++ " = " ++ prin b ++ ")"

print_val :: Val -> String
print_val VInvalid = "invalid"
print_val (VBool x) = show x
print_val (VInt x) = show x

data Val
    = VInvalid
    | VBool Bool
    | VInt Int
    deriving (Read, Show)

val_plus :: Val -> Val -> Val
val_plus (VInt a) (VInt b) = VInt (a + b)
val_plus _ _ = VInvalid

val_eq :: Val -> Val -> Val
val_eq (VInt a) (VInt b) = VBool (a == b)
val_eq (VBool a) (VBool b) = VBool (a == b)
val_eq _ _ = VInvalid

-- How do we compose Val and Exp?

data Lang
    = LVal Val
    | LExp (Exp Lang)
    deriving (Read, Show)

eval_lang :: Lang -> Val
eval_lang (LVal x) = x
eval_lang (LExp x) = eval_val (fmap eval_lang x)

render_lang :: Lang -> String
render_lang (LVal x) = print_val x
render_lang (LExp x) = render_exp render_lang x

example1 = eval_lang (LExp (EPlus (LVal (VInt 1)) (LVal (VInt 2))))
example2 = render_lang (LExp (EPlus (LExp (EPlus (LVal (VInt 1)) (LVal (VInt 2)))) (LVal (VInt 2))))
