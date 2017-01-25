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
import qualified Parse.Haskell.Token as T
import qualified Parse.Haskell.Untoken as U
import qualified Parse.Monad as M
import qualified Parse.Monad.Parsec as N

class (M.MonadParse m) => MonadParseHaskell m where

    token :: (L.Located T.Lexeme -> Maybe a) -> m a

instance MonadParseHaskell (N.Parsec [L.Located T.Lexeme]) where

    token match = N.token show L.locate match

theKeyword = token . U.theKeyword

theVarId = token . U.theVarId

kModule = theKeyword "module"
kWhere = theKeyword "where"
kImport = theKeyword "import"
kQualified = theVarId "qualified"
kAs = theVarId "as"

-- | Module identifier.
modId = token $ fmap U.unparse . U.anyQConId

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
