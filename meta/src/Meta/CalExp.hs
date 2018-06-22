module Meta.CalExp where

import Prelude hiding (exp)

import qualified Meta.CalVal as V

data Exp a
    = Pure a
    | Plus a a
    | Eq a a
    | Not a
    deriving (Read, Show)

instance Functor Exp where
    fmap f (Pure a) = Pure (f a)
    fmap f (Plus a b) = Plus (f a) (f b)
    fmap f (Eq a b) = Eq (f a) (f b)
    fmap f (Not a) = Not (f a)

-- This seems to be Traversable.sequenceA.
flipA :: (Applicative f) => Exp (f V.Val) -> f (Exp V.Val)
flipA (Pure a) = Pure <$> a
flipA (Plus a b) = Plus <$> a <*> b
flipA (Eq a b) = Eq <$> a <*> b
flipA (Not a) = Not <$> a

eval :: Exp V.Val -> V.Val
eval exp = case exp of
    Pure a -> a
    Plus a b -> V.plus a b
    Eq a b -> V.eq a b
    Not a -> V.not a

render :: (a -> String) -> Exp a -> String
render prin exp = case exp of
    Pure a -> prin a
    Plus a b -> "(" ++ prin a ++ " + " ++ prin b ++ ")"
    Eq a b -> "(" ++ prin a ++ " = " ++ prin b ++ ")"
    Not a -> "(not " ++ prin a ++ ")"
