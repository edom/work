{-# LANGUAGE PatternSynonyms #-}

module Meta.Cal where

import Prelude ()
import Meta.Prelude

import qualified Meta.CalExp as E
import qualified Meta.CalVal as V

-- * Term

data Lang
    = LVal V.Val
    | LExp (E.Exp Lang)
    deriving (Read, Show)

class CVal a where
    vError :: [String] -> a
    vInt :: Int -> a
    vBool :: Bool -> a

instance CVal V.Val where
    vError = V.Error
    vInt = V.Int
    vBool = V.Bool

instance CVal Lang where
    vError = LVal . vError
    vInt = LVal . vInt
    vBool = LVal . vBool

-- * Term constructors

plus :: Lang -> Lang -> Lang
plus a b = LExp (E.Plus a b)

eq :: Lang -> Lang -> Lang
eq a b = LExp (E.Eq a b)

-- * Term deconstructors

pattern PInt :: Int -> Lang
pattern PInt a = LVal (V.Int a)

-- * Evaluation

eval :: Lang -> V.Val
eval (LVal x) = V.eval x
eval (LExp x) = E.eval (eval <$> x)

-- * Rendering

render :: Lang -> String
render (LVal x) = V.print x
render (LExp x) = E.render render x

-- * Examples

example1 :: V.Val
example1 = eval (LExp (E.Plus (LVal (V.Int 1)) (LVal (V.Int 2))))

example2 :: String
example2 = render (LExp (E.Plus (LExp (E.Plus (LVal (V.Int 1)) (LVal (V.Int 2)))) (LVal (V.Int 2))))

example3 :: V.Val
example3 = eval (vInt 1 `plus` vInt 2)
