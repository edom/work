module Parse.Haskell.Layout
(
    -- * Before removing layout
    LToken(..)
    , makeLInput
    -- * Removing layout
    , unlayout
)
where

import qualified Parse.Haskell.Token as T
import qualified Parse.Haskell.Untoken as U
import qualified Parse.Location as L

data LToken
    = Normal T.Token
    | Brace Int -- ^ Haskell report uses the term @{ n }@ for this.
    | Angle Int -- ^ Haskell report uses the term @< n >@ for this.
    deriving (Read, Show)

makeLInput :: [L.Located T.Token] -> [L.Located LToken]
makeLInput = f
    where
        f [] = []
        f (L.MkLocated a x : L.MkLocated b y : z)
            | Just k <- U.anyKeyword x
            , k `elem` ["let", "where", "do", "of"]
            , Nothing <- U.leftBrace y
            = L.MkLocated a (Normal x)
            : L.MkLocated b (Brace $ L.column b)
            : f (L.MkLocated b y : z)
        -- TODO the rest
        f (L.MkLocated a x : y)
            = L.MkLocated a (Normal x) : f y

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
