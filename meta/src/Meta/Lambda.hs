module Meta.Lambda where

import Prelude hiding (exp)

type Name = String

data Exp a
    = Pure a
    | Var Name
    | Lam Name (Exp a)
    | App (Exp a) (Exp a)
    deriving (Show, Read)

subst
    :: Name -- ^ needle
    -> Exp a -- ^ replacement
    -> Exp a -- ^ haystack
    -> Exp a

subst name arg body = case body of
    Var n | n == name -> arg
    Lam n bod | n /= name -> Lam n (recur bod)
    App fn ar -> App (recur fn) (recur ar)
    _ -> body
    where
        recur = subst name arg
