{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Haskell.Parse
(
    -- * Class

    MonadParseHaskell(..)

    -- * Type

    , Module(..)
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

import qualified Parse.Location as L
import qualified Parse.Haskell.Lex as K
import qualified Parse.Monad as M
import qualified Parse.Monad.Parsec as N

class (M.MonadParse m) => MonadParseHaskell m where

    token :: (L.Located K.Lexeme -> Maybe a) -> m a

instance MonadParseHaskell (N.Parsec [L.Located K.Lexeme]) where

    token match = N.token show L.locate match

keyword s =
    token $ \ t -> do
        case t of
            L.MkLocated _ (K.Reserved str) | s == str -> Just s
            _ -> Nothing

varId s =
    token $ \ t -> do
        case t of
            L.MkLocated _ (K.QVarId "" str) | s == str -> Just s
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
            L.MkLocated _ (K.QConId prefix name) -> Just $ prefix ++ name
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
