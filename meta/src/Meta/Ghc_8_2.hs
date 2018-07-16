module Meta.Ghc_8_2 (
    GhcPs
    , ModuleGraph
    , getModuleGraph
    , mgModSummaries
) where

import qualified GHC as G
import qualified RdrName as R

type GhcPs = R.RdrName

newtype ModuleGraph = MkModuleGraph [G.ModSummary]

getModuleGraph :: (G.GhcMonad m) => m ModuleGraph
getModuleGraph = G.getModuleGraph

mgModSummaries :: ModuleGraph -> [G.ModSummary]
mgModSummaries (MkModuleGraph x) = x
