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
import qualified Data.List as DL

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
    insertIndent
    . insertBrace
    . insertFirstToken
    . map (fmap Normal)

insertIndent tokens =
    concatMap doInsertIndent theLines
    where
        theLines = DL.groupBy (\ x y -> L.getLine x == L.getLine y) tokens

doInsertIndent :: [L.Located (Laid T.Token)] -> [L.Located (Laid T.Token)]
doInsertIndent x@(L.MkLocated _ (Brace _) : _)
    = x
doInsertIndent (x : y)
    | U.isLexeme x
    = L.MkLocated (L.locate x) (Angle (L.getColumn x)) : x : y
doInsertIndent (x : y)
    = x : doInsertIndent y
doInsertIndent x
    = x

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
unlayout tokens = doUnlayout (prepare tokens) []

-- TODO
-- This is the L function described in the report.

doUnlayout :: [L.Located (Laid T.Token)] -> [Int] -> [L.Located T.Token]

doUnlayout (t : ts) (m : ms)
    | Just n <- angle t
    , m == n
    = like t colon : doUnlayout ts (m : ms)

doUnlayout (t : ts) (m : ms)
    | Just n <- angle t
    , n < m
    = like t rightBrace : doUnlayout (like t (Angle n) : ts) ms

doUnlayout (t : ts) ms
    | Just _ <- angle t
    = doUnlayout ts ms

doUnlayout (t : ts) (m : ms)
    | Just n <- brace t
    , n > m
    = like t leftBrace : doUnlayout ts (n : m : ms)

doUnlayout (t : ts) []
    | Just n <- brace t
    = like t leftBrace : doUnlayout ts [n]

-- Note 2
doUnlayout (t : ts) ms
    | Just n <- brace t
    = like t leftBrace : like t rightBrace : doUnlayout (like t (Angle n) : ts) ms

-- Note 3
doUnlayout (t : ts) (0 : ms)
    | Just _ <- U.rightBrace t
    = like t rightBrace : doUnlayout ts ms

-- Note 3
doUnlayout (t : ts) ms
    | Just _ <- U.rightBrace t
    = error "shit"

-- Note 4
doUnlayout (t : ts) ms
    | Just _ <- U.leftBrace t
    = like t leftBrace : doUnlayout ts (0 : ms)

-- Note 5 is too complex to implement.

doUnlayout (t : ts) ms
    | L.MkLocated p (Normal u) <- t
    = L.MkLocated p u : doUnlayout ts ms

doUnlayout [] [] = []

-- Note 6
doUnlayout [] (m : ms) = L.MkLocated (L.MkLocation "" 0 0) rightBrace : doUnlayout [] ms

doUnlayout list stack = [ L.MkLocated a x | L.MkLocated a (Normal x) <- list ]

colon = T.TLexeme $ T.Special ';'

leftBrace = T.TLexeme $ T.Special '{'
rightBrace = T.TLexeme $ T.Special '}'

angle (L.MkLocated _ (Angle n)) = Just n
angle _ = Nothing

brace (L.MkLocated _ (Brace n)) = Just n
brace _ = Nothing

like (L.MkLocated p _) x = L.MkLocated p x
