{- !
https://en.wikipedia.org/wiki/SKI_combinator_calculus
-}
module Meta.Ski where

data Term a
    = Pure a
    | I
    | K
    | S
    | App (Term a) (Term a)
    deriving (Read, Show)

infixl 8 `App`

i :: Term a -> Term a
i x = I `App` x

k :: Term a -> Term a -> Term a
k x y = App (App K x) y

s :: Term a -> Term a -> Term a -> Term a
s x y z = App (App (App S x) y) z

instance Functor Term where
    fmap f (Pure a) = Pure (f a)
    fmap _ I = I
    fmap _ K = K
    fmap _ S = S
    fmap f (App x y) = App (fmap f x) (fmap f y)

eval :: Term a -> Term a
eval = recur
    where
        recur I = I
        recur K = K
        recur S = S
        recur (App I x) = recur x
        recur (App (App K x) _) = recur x
        recur (App (App (App S x) y) z) =
            let
                rx = recur x
                ry = recur y
                rz = recur z
            in
                recur $ App (recur $ App rx rz) (recur $ App ry rz)
        recur x = x
