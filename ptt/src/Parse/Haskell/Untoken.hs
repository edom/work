module Parse.Haskell.Untoken
(
    Untoken(..)
)
where

import qualified Control.Applicative as A

import qualified Parse.Location as L
import qualified Parse.Haskell.Token as T

{- |
This allows you to pattern-match on 'T.Token'-like things
without tightly coupling to the actual constructors.
-}
class Untoken a where

    -- | Match a reserved keyword.
    keyword :: (A.Alternative f) => a -> f String

    -- | Match a left brace.
    leftBrace :: (A.Alternative f) => a -> f String

instance Untoken T.Whitespace where
    keyword _ = A.empty
    leftBrace _ = A.empty

instance Untoken T.Lexeme where
    keyword (T.Reserved x) = pure x
    keyword _ = A.empty
    leftBrace (T.Reserved x@"{") = pure x
    leftBrace _ = A.empty

instance (Untoken a) => Untoken (L.Located a) where
    keyword (L.MkLocated _ x) = keyword x
    leftBrace (L.MkLocated _ x) = leftBrace x

instance (Untoken a, Untoken b) => Untoken (Either a b) where
    keyword = either keyword keyword
    leftBrace = either leftBrace leftBrace
