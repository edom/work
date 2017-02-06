{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Haskell.Syntax
(
    IsModule(..)
    , IsDecl(..)
    , sloppyParseFile
)
where

import qualified Control.Applicative as A

import qualified Language.Haskell.Exts as P

import qualified Haskell.Interpret.Expression as E

class IsModule t where
    getDeclarations :: t -> [E.Decl]

instance IsModule P.Module where
    getDeclarations (P.Module _srcloc _name _pragmas _mWarning _mExports _imports decls) =
        [
            E.MkDecl name (toExpr rhs)
            | P.PatBind _loc lhs rhs _binds <- decls
            , P.PVar (P.Ident name) <- return lhs
        ]

class IsDecl t where
    -- | Left-hand side.
    getLhs :: t -> String
    -- | Right-hand side.
    getRhs :: t -> E.Expr

instance IsDecl E.Decl where
    getLhs (E.MkDecl x _) = x
    getRhs (E.MkDecl _ x) = x

class ToExpr t where
    toExpr :: t -> E.Expr

instance ToExpr P.Rhs where
    toExpr (P.UnGuardedRhs expr) = toExpr expr
    toExpr x = notImplemented x

instance ToExpr P.Exp where
    toExpr (P.App f x) = E.App (toExpr f) (toExpr x)
    toExpr (P.Var (P.UnQual (P.Ident x))) = E.Var x
    toExpr (P.InfixApp x op y) = E.App (E.App (toExpr op) (toExpr x)) (toExpr y)
    toExpr (P.Lit x) = toExpr x
    toExpr x = notImplemented x

instance ToExpr P.QOp where
    toExpr (P.QVarOp (P.UnQual (P.Symbol x))) = E.Var x
    toExpr x = notImplemented x

notImplemented x = E.Err $ "not implemented: toExpr " ++ show x

instance ToExpr P.Literal where
    toExpr (P.Int x) | x == up = E.Int down
        where
            down :: Int
            down = fromIntegral x
            up :: Integer
            up = fromIntegral down
    toExpr (P.Int x) = E.Err $ "integer overflow while downsizing Integer into Int:" ++ show x
    toExpr x = notImplemented x

sloppyParseFile :: FilePath -> IO P.Module
sloppyParseFile path = P.fromParseResult <$> P.parseFile path

-- TODO encode a module as a @(String -> m Expr) -> (String -> m Expr)@;
-- the input is the dependency resolver
-- the output is the resolver;
-- a module is a resolver that is parameterized by another resolver
-- if a module has an empty import list, then its input resolver is @\ s -> Err $ "undefined variable " ++ s@
-- if a module has n import lists then its input resolver is @\ s -> m0 s '<|>' m1 s '<|>' m2 s '<|>' ...@
-- import M = f
-- import qualified M as P = \ s -> if "P." `isPrefixOf s then f s else Err "this module does not begin with that prefix"

empty :: (Monad m) => String -> String -> m E.Expr
empty modName varName = fail $ "module " ++ modName ++ " does not export " ++ varName

add :: (A.Alternative m) => (String -> m E.Expr) -> (String -> m E.Expr) -> (String -> m E.Expr)
add f g s = f s A.<|> g s

fromModule :: (Monad m) => P.Module -> String -> m E.Expr
fromModule module_ varName =
    case [ getRhs d | d <- decls, getLhs d == varName ] of
        [x] -> pure x
        [] -> fail $ "Module " ++ modName ++ " does not export " ++ varName
        _ -> fail $ "Invalid input: Module " ++ modName ++ " exports more than one " ++ varName
    where
        modName = "MODNAME"
        decls = getDeclarations module_

-- fold? add (map fromModule imports) (empty modName)?
