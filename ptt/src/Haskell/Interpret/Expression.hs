-- | Interpretation-oriented representation.

module Haskell.Interpret.Expression
(
    Expr(..)
    , Module(..)
    , Decl(..)
    , reduce
)
where

import qualified Lambda.Term as N
import qualified Haskell.Interpret.Name as Q

data Expr
    = Lam String Expr
    | App Expr Expr
    | Var String
    | Int Int
    | IntAdd
    | Double Double
    | DoubleAdd
    | Err String
    | IntToDouble
    deriving (Read, Show, Eq)

reduce :: (Monad m) => [(String, Expr)] -> Expr -> m Expr
reduce bindings = go
    where

        go (App IntToDouble (Int x)) = pure $ Double $ fromIntegral x
        go (App f x) | f `elem` prim1 = App <$> go f <*> pure x

        go (App (App IntAdd (Int x)) (Int y)) = pure $ Int (x + y)
        go (App (App DoubleAdd (Double x)) (Double y)) = pure $ Double (x + y)
        go (App (App f x) y) | f `elem` prim2 = App <$> (App f <$> go x) <*> go y

        go (App (Lam name body) arg) = pure $ substitute name arg body

        go (App f x) = App <$> go f <*> pure x

        go (Var name) = find name bindings
        go (Err msg) = fail msg
        go x = fail $ "cannot reduce " ++ show x

        prim1 = [IntToDouble]
        prim2 = [IntAdd, DoubleAdd]

find name bindings =
    check name [ e | (n, e) <- bindings, n == name ]
    where
        check name [x] = pure x
        check name [] = fail $ "not found: " ++ name
        check name _ = fail $ "ambiguous: " ++ name

substitute :: String -> Expr -> Expr -> Expr
substitute name rep = go
    where
        go (Var n) | name == n = rep
        go (App a b) = App (go a) (go b)
        go (Lam n body) | name /= n = Lam n (go body)
        go x = x

instance N.Term Expr where

    mkApp = App

    unApp t _ (App x y) = t x y
    unApp _ f x = f x

    mkLam = Lam

    unLam t _ (Lam x y) = t x y
    unLam _ f x = f x

    mkVar = Var

    unVar t _ (Var x) = t x
    unVar _ f x = f x

class HasName a where
    getName :: a -> String

data Module
    = MkModule
    {
        mName :: String
        , decls :: [Decl]
    }
    deriving (Read, Show)

instance HasName Module where
    getName = mName

data Decl
    = MkDecl
    {
        dName :: String
        , dExpr :: Expr
    }
    deriving (Read, Show)

empty :: String -> Module
empty name_ = MkModule name_ []
