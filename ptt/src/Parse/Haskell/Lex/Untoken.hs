{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse.Haskell.Lex.Untoken
(
    Name(..)
    , unparse
    , Untoken(..)
    , anyVarId
    , anyConId
    , theKeyword
    , theVarId
    , theConId
)
where

import qualified Control.Applicative as A
import qualified Control.Monad as M

import qualified Parse.Location as L
import qualified Parse.Haskell.Lex.Token as T

-- | Qualified name.
data Name
    = MkName String String
    deriving (Read, Show)

unparse :: Name -> String
unparse (MkName x y) = x ++ y

asUnqualified :: (A.Alternative f) => Name -> f String
asUnqualified (MkName "" x) = pure x
asUnqualified _ = A.empty

unqualified :: Name -> String
unqualified (MkName _ x) = x

eq :: (A.Alternative f, Eq a) => a -> a -> f a
eq x y | x == y = pure x
eq _ _ = A.empty

{- |
This allows you to pattern-match on 'T.Token'-like things
without tightly coupling to the actual constructors.
-}
class Untoken a where

    -- | Match a reserved keyword.
    anyKeyword :: (M.MonadPlus f) => a -> f String

    -- | Match a left brace.
    leftBrace :: (M.MonadPlus f) => a -> f String

    -- | Match a right brace.
    rightBrace :: (M.MonadPlus f) => a -> f String

    -- | Match a 'T.QVarId'.
    anyQVarId :: (M.MonadPlus f) => a -> f Name

    -- | Match a 'T.QConId'.
    anyQConId :: (M.MonadPlus f) => a -> f Name

-- | Match an unqualified identifier that begins with a lowercase character.
anyVarId = anyQVarId M.>=> asUnqualified

anyConId = anyQConId M.>=> asUnqualified

theVarId x = anyVarId M.>=> eq x

theConId x = anyConId M.>=> eq x

theKeyword x = anyKeyword M.>=> eq x

instance Untoken T.Whitespace where
    anyKeyword _ = A.empty
    leftBrace _ = A.empty
    rightBrace _ = A.empty
    anyQVarId _ = A.empty
    anyQConId _ = A.empty

instance Untoken T.Lexeme where
    anyKeyword (T.Reserved x) = pure x
    anyKeyword _ = A.empty
    leftBrace (T.Special '{') = pure "{"
    leftBrace _ = A.empty
    rightBrace (T.Special '}') = pure "}"
    rightBrace _ = A.empty
    anyQVarId (T.QVarId qual name) = pure $ MkName qual name
    anyQVarId _ = A.empty
    anyQConId (T.QConId qual name) = pure $ MkName qual name
    anyQConId _ = A.empty

instance (Untoken a) => Untoken (L.Located a) where
    anyKeyword (L.MkLocated _ x) = anyKeyword x
    leftBrace (L.MkLocated _ x) = leftBrace x
    rightBrace (L.MkLocated _ x) = rightBrace x
    anyQVarId (L.MkLocated _ x) = anyQVarId x
    anyQConId (L.MkLocated _ x) = anyQConId x

instance (Untoken a, Untoken b) => Untoken (Either a b) where
    anyKeyword = either anyKeyword anyKeyword
    leftBrace = either leftBrace leftBrace
    rightBrace = either rightBrace rightBrace
    anyQVarId = either anyQVarId anyQVarId
    anyQConId = either anyQConId anyQConId
