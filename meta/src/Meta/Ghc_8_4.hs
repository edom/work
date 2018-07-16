module Meta.Ghc_8_4 (
    G.GhcPs
    , T.ModuleGraph
    , getModuleGraph
    , G.mgModSummaries
) where

import qualified GHC as G
import qualified HscTypes as T

getModuleGraph :: (G.GhcMonad m) => m T.ModuleGraph
getModuleGraph = G.getModuleGraph
