{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parse.Haskell.Parse
(
    -- * Class

    MonadParseHaskell(..)

    -- * Syntax

    , module_
    , body
    , impDecl
    , exports
)
where

import qualified Control.Applicative as A
import qualified Data.Maybe as Q

import Parse.Monad
    (
        (<|>)
    )

import qualified Parse.Location as L
import qualified Parse.Haskell.Abstract as C
import qualified Parse.Haskell.Lex as K
import qualified Parse.Haskell.Lex.Token as T
import qualified Parse.Haskell.Lex.Untoken as U
import qualified Parse.Monad as M
import qualified Parse.Monad.Parsec as N

class (M.MonadParse m) => MonadParseHaskell m where

    token :: (L.Located T.Lexeme -> Maybe a) -> m a

instance MonadParseHaskell (N.Parsec [L.Located T.Lexeme]) where

    token match = N.token show L.locate match

theKeyword = token . U.theKeyword

theVarId = token . U.theVarId

leftBrace = token U.leftBrace
rightBrace = token U.rightBrace

kModule = theKeyword "module"
kWhere = theKeyword "where"
kImport = theKeyword "import"
kQualified = theVarId "qualified"
kAs = theVarId "as"

-- | Module identifier.
modId = token $ fmap U.unparse . U.anyQConId

module_ = do
    name <- kModule *> modId
    imports <- kWhere *> body
    return $ C.MkModule name Nothing imports []

-- | Module body.
body = leftBrace *> content
    where
        content =
            (rightBrace *> pure [])
            <|> (impDecl <:> content)

(<:>) = A.liftA2 (:)

-- | Import declaration.
impDecl = do
    kImport
    mQualified <- M.optional kQualified
    moduleId <- modId
    mAlias <- M.optional $ kAs >> modId
    return $ C.MkImport (Q.isJust mQualified) moduleId (Q.fromMaybe "" mAlias)

exports = do
    M.char '('
    M.many (export >> M.char ',')
    M.char ')'

export = qVar

type ModId = String
type VarId = String
data QVar = MkQVar (Maybe ModId) VarId

qVar = K.qVarId <|> (M.char '(' *> K.qVarSym <* M.char ')')
