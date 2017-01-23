{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Haskell.Parse
(
    -- * Type

    Module(..)
    , Import(..)

    -- * Syntax

    , module_
    , body
    , impDecl
    , exports
)
where

import Data.Maybe as Q

import Parse.Monad
    (
        (<|>)
    )

import qualified Parse.Monad as M
import qualified Parse.Monad.Parsec as N

import qualified Parse.Haskell.Lex as K

token match = N.token show K.locate (\ (K.MkLocated _ x) -> match x)

keyword s =
    token $ \ t -> do
        case t of
            K.Reserved str | s == str -> Just s
            _ -> Nothing

varId s =
    token $ \ t -> do
        case t of
            K.QVarId "" str | s == str -> Just s
            _ -> Nothing

kModule = keyword "module"
kWhere = keyword "where"
kImport = keyword "import"
kQualified = varId "qualified"
kAs = varId "as"

-- | Module identifier.
modId =
    token $ \ t -> do
        case t of
            K.QConId prefix name -> Just $ prefix ++ name
            _ -> Nothing

data Module
    = MkModule String [Import]
    deriving (Read, Show)

data Import
    = MkImport
    {
        iQualified :: Bool
        , iModule :: String
        , iAlias :: String
    }
    deriving (Read, Show)

module_ =
    MkModule <$> (kModule *> modId) <*> (kWhere *> body)
    -- M.optional exports

-- | Module body.
body = M.many impDecl

-- | Import declaration.
impDecl = do
    kImport
    mQualified <- M.optional kQualified
    moduleId <- modId
    mAlias <- M.optional $ kAs >> modId
    return $ MkImport (Q.isJust mQualified) moduleId (Q.fromMaybe "" mAlias)

exports = do
    M.char '('
    M.many (export >> M.char ',')
    M.char ')'

export = qVar

type ModId = String
type VarId = String
data QVar = MkQVar (Maybe ModId) VarId

qVar = K.qVarId <|> (M.char '(' *> K.qVarSym <* M.char ')')
