module Meta.GhcExample where

import Prelude ()
import Meta.Prelude

import qualified GHC as F
import qualified Meta.Ghc as G

example_ghc :: IO ()
example_ghc =
    G.defaultRunGhc $ do

        flags <- G.getSessionDynFlags
        _ <- G.setSessionDynFlags $ flags {
                G.ghcLink = G.NoLink
                , G.hscTarget = G.HscNothing
            }
            |> ignore_package_environment

        G.addFile "test/Foo.hs"
        suc <- G.loadAllTargets

        when (G.succeeded suc) $ do
            graph <- G.getModuleGraph
            forM_ (G.mgModSummaries graph) $ \ ms -> do
                pm <- G.parseModule ms
                putStrLn $ "-- This is a module parsed from file: " ++ G.getName ms
                let hsmod = G.unLoc $ G.pm_parsed_source pm
                G.prettyPrint hsmod
                return ()

        putStrLn "-- This is a module built from scratch."
        let
            mod = G.pass_GhcPs (G.emptyHsModule {
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
            doc = G.ppr mod
        putStrLn $ G.showSDoc flags doc

{- |
These package-flag shenanigans are required to work around the unholy marriage between cabal and ghc:

* If we run @cabal new-repl meta@ without having running @cabal new-build meta@ beforehand
(because we have just checked out the repository, or because we have just nuked the @dist-newstyle@ directory):

    * @cabal new-repl meta@ generates a @.ghc.environment.ARCH-OS-GHCVER@ file.

    * GHC reads that file and fails with a message like \"cannot satisfy -package-id meta-0.0.0-inplace\".

    * We can't just run @ghc Foo.hs@ in that directory.
    Instead we have to run @ghc -hide-all-packages -clear-package-db -global-package-db -user-package-db -package base Foo.hs@.
    Or we can just run @cabal new-build meta@ first, which puts @meta-0.0.0-inplace@ in a package database somewhere in @dist-newstyle@.

    * We don't want @cabal new-repl@ to run @new-build@ as it will slow down @new-repl@.
-}
ignore_package_environment :: G.DynFlags -> G.DynFlags
ignore_package_environment flags = flags {
        G.packageDBFlags = reverse [G.ClearPackageDBs, G.PackageDB G.GlobalPkgConf, G.PackageDB G.UserPkgConf]
        , G.packageFlags = reverse [G.ExposePackage "base" (G.PackageArg "base") (G.ModRenaming True [])]
    }
    |> G.setGeneralFlag G.Opt_HideAllPackages
