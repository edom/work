module Parse.Haskell.Layout
(
    -- * Before removing layout
    LToken(..)
    , makeLToken
    -- * Removing layout
    , unlayout
)
where

import qualified Control.Applicative as A
import qualified Control.Monad as M

import qualified Parse.Haskell.Token as T
import qualified Parse.Haskell.Untoken as U
import qualified Parse.Location as L

data LToken
    = Normal T.Lexeme
    | Brace Int -- ^ Haskell report uses the term @{ n }@ for this.
    | Angle Int -- ^ Haskell report uses the term @< n >@ for this.
    deriving (Read, Show)

asLexeme :: (M.MonadPlus f) => LToken -> f T.Lexeme
asLexeme (Normal x) = pure x
asLexeme _ = A.empty

instance U.Untoken LToken where
    anyKeyword = asLexeme M.>=> U.anyKeyword
    leftBrace = asLexeme M.>=> U.leftBrace
    rightBrace = asLexeme M.>=> U.rightBrace
    anyQVarId = asLexeme M.>=> U.anyQVarId
    anyQConId = asLexeme M.>=> U.anyQConId

makeLToken :: [L.Located T.Lexeme] -> [L.Located LToken]
makeLToken = f . (\ x -> insertFirstToken (take 1 x) ++ map (fmap Normal) x)
    where
        -- Insert Angle if the first token is not @{@ and is not @module@.
        insertFirstToken :: [L.Located T.Lexeme] -> [L.Located LToken]
        insertFirstToken maybeHead = do
            x <- maybeHead
            Just _ <- return $ U.leftBrace x A.<|> U.theKeyword "module" x
            return $ L.MkLocated (L.locate x) $ Angle (L.column $ L.locate x)
        f :: [L.Located LToken] -> [L.Located LToken]
        f [] = []
        f (L.MkLocated a x : L.MkLocated b y : z)
            | Just k <- U.anyKeyword x
            , k `elem` ["let", "where", "do", "of"]
            , Nothing <- U.leftBrace y
            = L.MkLocated a x
            : L.MkLocated b (Brace $ L.column b)
            : f (L.MkLocated b y : z)
        -- TODO the rest
        f (L.MkLocated a x : y)
            = L.MkLocated a x : f y

{- |
Insert braces and semicolons implied by indentations.

This should be idempotent (@unlayout . unlayout = unlayout@).

See <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html#x17-17800010.3 Layout> (Haskell 2010 Report).
-}
unlayout :: [L.Located LToken] -> [L.Located LToken]
unlayout tokens = f tokens []
    where
        -- This f is L in the Haskell report.
        f x _ = x -- TODO
