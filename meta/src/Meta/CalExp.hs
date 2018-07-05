module Meta.CalExp where

import Prelude ()
import Meta.Prelude

import qualified Meta.CalVal as V

{- |
Calculator expression language: arithmetics and conditional.

This expression language is contained in every widely used programming languages.
-}
data Exp a
    = Pure a -- ^ constant
    | Plus a a -- ^ addition
    | Eq a a -- ^ equality comparison
    | Not a -- ^ logical negation
    | If a a a -- ^ If boolCond truePart falsePart
    deriving (Read, Show)

instance Functor Exp where
    fmap f (Pure a) = Pure (f a)
    fmap f (Plus a b) = Plus (f a) (f b)
    fmap f (Eq a b) = Eq (f a) (f b)
    fmap f (Not a) = Not (f a)
    fmap f (If a b c) = If (f a) (f b) (f c)

-- This seems to be Traversable.sequenceA.
flipA :: (Applicative f) => Exp (f V.Val) -> f (Exp V.Val)
flipA (Pure a) = Pure <$> a
flipA (Plus a b) = Plus <$> a <*> b
flipA (Eq a b) = Eq <$> a <*> b
flipA (Not a) = Not <$> a
flipA (If a b c) = If <$> a <*> b <*> c

eval :: Exp V.Val -> V.Val
eval exp = case exp of
    Pure a -> a
    Plus a b -> V.plus a b
    Eq a b -> V.eq a b
    Not a -> V.not a
    If c t f -> V.if_ c t f

render :: (a -> String) -> Exp a -> String
render prin exp = case exp of
    Pure a -> prin a
    Plus a b -> "(" ++ prin a ++ " + " ++ prin b ++ ")"
    Eq a b -> "(" ++ prin a ++ " = " ++ prin b ++ ")"
    Not a -> "(not " ++ prin a ++ ")"
    If c t f -> "(if " ++ prin c ++ " " ++ prin t ++ " " ++ prin f ++ ")"
