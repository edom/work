{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskell.Interpret
(
    Expr
    , reduce
)
where

import qualified Control.Applicative as A
import qualified Control.Monad as M

import Haskell.Interpret.Expression
import Lambda.Deconstruct

import qualified Haskell.Syntax as S
import qualified Lambda.Term as N
import qualified Lambda.Reduce as R

class (N.Term t) => Term3 t where
    unInt :: (Int -> a) -> (t -> a) -> (t -> a)
    unIntAdd :: (t -> a) -> (t -> a) -> (t -> a)
    unDouble :: (Double -> a) -> (t -> a) -> (t -> a)
    unDoubleAdd :: (t -> a) -> (t -> a) -> (t -> a)
    err :: (A.Alternative m) => t -> m String
    mkIntAdd :: t
    mkInt :: Int -> t
    mkDouble :: Double -> t
    mkDoubleAdd :: t

instance Term3 Expr where

    unInt t _ (Int x) = t x
    unInt _ f x = f x

    unIntAdd t _ IntAdd = t IntAdd
    unIntAdd _ f x = f x

    unDouble t _ (Double x) = t x
    unDouble _ f x = f x

    unDoubleAdd t _ DoubleAdd = t DoubleAdd
    unDoubleAdd _ f x = f x

    err (Err x) = pure x
    err _ = A.empty

    mkInt = Int
    mkIntAdd = IntAdd
    mkDouble = Double
    mkDoubleAdd = DoubleAdd

int :: (A.Alternative m, Term3 t) => t -> m Int
int = unInt pure (const A.empty)

intAdd :: (A.Alternative m, Term3 t) => t -> m t
intAdd = unIntAdd pure (const A.empty)

double :: (A.Alternative m, Term3 t) => t -> m Double
double = unDouble pure (const A.empty)

doubleAdd :: (A.Alternative m, Term3 t) => t -> m t
doubleAdd = unDoubleAdd pure (const A.empty)

binPrimOp t = intAdd t <|> doubleAdd t

one :: (Monad m) => String -> String -> [a] -> m a
one _ _ [x] = return x
one notFoundMsg _ [] = fail notFoundMsg
one _ ambiguousMsg _ = fail ambiguousMsg

oneName name_ list = one ("not found: " ++ name_) ("ambiguous: " ++ name_) list

-- | Step a Haskell expression.
reduce2 :: (M.Monad m, Term3 t, Show t) => [(String, t)] -> t -> m t
reduce2 bindings t =
    var t
        |> (\ name_ -> oneName name_ [ term | (n, term) <- bindings, n == name_])
    <|>
    app (app intAdd int) int t
        |> (\ ((_, x), y) -> pure $ mkInt $ x + y)
    <|>
    app (app doubleAdd double) double t
        |> (\ ((_, x), y) -> pure $ mkDouble $ x + y)
    <|>
    app (app binPrimOp pure) pure t
        |> (\ ((op, x), y) -> do
            u <- reduce2 bindings op
            v <- reduce2 bindings x
            w <- reduce2 bindings y
            pure $ N.mkApp (N.mkApp u v) w)
    <|>
    app pure pure t
        |> (\ (fun, arg) -> do
            fun' <- reduce2 bindings fun
            pure $ N.mkApp fun' arg)
    <|>
    err t |> fail
    <|>
    R.reduce t |> pure
    <!> fail ("does not know how to reduce " ++ show t ++ " with bindings " ++ show bindings)

testLambda = reduces 8 term
    where
        -- examples:
        -- term = Err "error"
        -- term = App (Err "f") (Int 0)
        -- term = App (App IntAdd (Int 5)) (Int 6)
        -- term = App (App (Lam "x" (Lam "y" (App (App DoubleAdd (Var "x")) (Var "y")))) (Double 1)) (Double 2)
        -- term = App (App (Lam "x" (Lam "y" (App (App IntAdd (Var "x")) (Var "y")))) (Int 1)) (Int 2)
        -- term = App (Lam "x" (Int 0)) (Err "in normal order, this doesn't raise an error")
        --
        -- (\ x -> x x) (\ x -> x x)
        -- term = App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x")))
        --
        -- (\ f x -> f x) (\ x -> x + 1) 2
        term =
            App
                (App
                    (Lam "f" (Lam "x" (App (Var "f") (Var "x"))))
                    (Lam "x"
                        (App
                            (App IntAdd (Var "x"))
                            (Int 1))))
                (Int 2)
        reduces n x | n <= 0 = do
            print x
            return x
        reduces n x = do
            print x
            case reduce2 [] x of
                Just y -> reduces (n - 1 :: Int) y
                _ -> pure x

-- | This interprets Test.hs.
testInterpret = do
    module_ <- S.sloppyParseFile "Test.hs"
    print module_
    let decls = S.getDeclarations module_
    main <- oneName "main" [ d | d <- decls, S.getLhs d == "main" ]
    rep 4 (dExpr main)
    where
        rep 0 x = print x
        rep n x = do
            y <- reduce bindings x
            print y
            rep (n - 1 :: Int) y
        bindings =
            [
                ("+#", IntAdd)
                , ("+##", DoubleAdd)
                , ("int2Double#", IntToDouble)
            ]
        asMaybe :: Maybe a -> Maybe a
        asMaybe = id
