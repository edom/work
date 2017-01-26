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
    isLexeme (Normal x) = U.isLexeme x
    isLexeme _ = True
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
    insertBrace
    . insertFirstToken
    . map (fmap Normal)

{-
If the lexeme after X is not brace, insert Brace n,
where
X is in [let, where, do, of]
and n is the indentation of the next lexeme or 0,
-}
insertBrace :: [L.Located (Laid T.Token)] -> [L.Located (Laid T.Token)]
insertBrace [] = []
insertBrace (x : y) | shouldBraceFollow x = x : doInsert x y
insertBrace (x : y) = x : insertBrace y

shouldBraceFollow x
    | Just k <- U.anyKeyword x
    , k `elem` ["let", "where", "do", "of"]
    = True
shouldBraceFollow _ = False

doInsert prev [] = [L.MkLocated (L.locate prev) (Brace 0)]
doInsert _ (s : t) | not (U.isLexeme s) = s : doInsert s t
doInsert _ (s : t) | Nothing <- U.leftBrace s = L.MkLocated (L.locate s) (Brace (L.getColumn s)) : s : insertBrace t
doInsert _ (s : t) = s : insertBrace t

-- Insert Brace if the first token is not @{@ or @module@.
insertFirstToken :: [L.Located (Laid T.Token)] -> [L.Located (Laid T.Token)]
insertFirstToken (x : y) | not (U.isLexeme x)
    = x : insertFirstToken y
insertFirstToken (x : y)
    | Nothing <- U.leftBrace x A.<|> U.theKeyword "module" x
    = L.MkLocated (L.locate x) (Brace (L.getColumn x)) : x : y
insertFirstToken x
    = x

{- |
Insert braces and semicolons implied by indentations.

This should be idempotent (@unlayout . unlayout = unlayout@).

See also the
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3 Layout>
section of the Haskell 2010 Report.
-}
unlayout :: [L.Located T.Token] -> [L.Located T.Token]
unlayout tokens = f (prepare tokens) []
    where
        -- This is the L function described in the report.
        f list _ = [ L.MkLocated a x | L.MkLocated a (Normal x) <- list ]
        -- TODO
