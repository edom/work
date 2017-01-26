{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Parse.Haskell.Layout
(
    -- * Before removing layout
    Laid(..)
    , prepare
    -- * Removing layout
    , unlayout
)
where

import qualified Control.Applicative as A
import qualified Control.Monad as M

import qualified Parse.Haskell.Lex.Token as T
import qualified Parse.Haskell.Lex.Untoken as U
import qualified Parse.Location as L

data Laid a
    = Normal a
    | Brace Int -- ^ Haskell report uses the term @\{n\}@ for this.
    | Angle Int -- ^ Haskell report uses the term @\<n\>@ for this.
    deriving (Read, Show)

instance Functor Laid where
    fmap f (Normal x) = Normal (f x)
    fmap _ (Brace x) = Brace x
    fmap _ (Angle x) = Angle x

instance U.Untoken (Laid T.Token) where
    anyKeyword = asLexeme M.>=> U.anyKeyword
    leftBrace = asLexeme M.>=> U.leftBrace
    rightBrace = asLexeme M.>=> U.rightBrace
    anyQVarId = asLexeme M.>=> U.anyQVarId
    anyQConId = asLexeme M.>=> U.anyQConId

unlay :: (A.Alternative f) => Laid a -> f a
unlay (Normal x) = pure x
unlay _ = A.empty

asLexeme = unlay M.>=> T.asLexeme

{- |
This transforms the output of the lexical analyzer to the input of the L function
according to the Haskell 2010 report.
-}
prepare :: [L.Located T.Token] -> [L.Located (Laid T.Token)]
prepare =
    f
    . insertFirstToken
    where
        f :: [L.Located (Laid T.Token)] -> [L.Located (Laid T.Token)]
        f [] = []
        f (x : y : z)
            | Just k <- U.anyKeyword x
            , k `elem` ["let", "where", "do", "of"]
            , Nothing <- U.leftBrace y
            =
            let
                loc = L.locate y
                col = L.column loc
            in
                x
                : L.MkLocated loc (Brace col)
                : f (y : z)
        -- TODO the rest
        f (x : y)
            = x : f y

-- Insert Angle if the first token is @{@ or @module@.
insertFirstToken :: [L.Located T.Token] -> [L.Located (Laid T.Token)]
insertFirstToken (x : y)
    | Just _ <- U.leftBrace x A.<|> U.theKeyword "module" x
    = L.MkLocated (L.locate x) (Angle (L.getColumn x)) : map (fmap Normal) (x : y)
insertFirstToken x
    = map (fmap Normal) x

{- |
Insert braces and semicolons implied by indentations.

This should be idempotent (@unlayout . unlayout = unlayout@).

See also the
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3 Layout>
section of the Haskell 2010 Report.
-}
unlayout :: [L.Located (Laid T.Token)] -> [L.Located (Laid T.Token)]
unlayout tokens = f tokens []
    where
        -- This is the L function described in the report.
        f x _ = x -- TODO
