module Meta.CalExp where

data Exp a
    = Pure a
    | Plus a a
    | Eq a a
    deriving (Read, Show)

instance Functor Exp where
    fmap f (Pure a) = Pure (f a)
    fmap f (Plus a b) = Plus (f a) (f b)
    fmap f (Eq a b) = Eq (f a) (f b)

render :: (a -> String) -> Exp a -> String
render prin exp = case exp of
    Pure a -> prin a
    Plus a b -> "(" ++ prin a ++ " + " ++ prin b ++ ")"
    Eq a b -> "(" ++ prin a ++ " = " ++ prin b ++ ")"
