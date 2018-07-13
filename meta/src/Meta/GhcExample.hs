module Meta.GhcExample where

import Prelude ()
import Meta.Prelude

import qualified GHC as F
import qualified Meta.Ghc as G

example_ghc :: IO ()
example_ghc = G.defaultRunGhc $ do
    flags <- G.getSessionDynFlags
    mod <- return $ G.pass_GhcPs (G.emptyHsModule {
            G.hsmodDecls = [
                G.noLoc $ F.TyClD (G.emptyDataDecl {
                        G.tcdDataDefn = G.emptyHsDataDefn {
                            F.dd_cons = [
                                G.noLoc (G.emptyCon |> G.setName "MyCon")
                            ]
                        }
                    }
                    |> G.setName "Foo"
                )
            ]
        }
        |> G.setName "Example"
        )
    doc <- return $ G.ppr mod
    putStrLn $ G.showSDoc flags doc
