module Meta.GhcExample where

import Prelude ()
import Meta.Prelude

import qualified Bag
import qualified DriverPhases
import qualified ErrUtils
import qualified HscMain
import qualified HscTypes
import qualified GHC as F
import qualified TcRnDriver
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
            putStrLn "Should we ignore loadAllTargets's return value?"

        when True $ do
            graph <- G.getModuleGraph
            forM_ (G.mgModSummaries graph) $ \ mod_sum -> do
                parsed_mod <- G.parseModule mod_sum
                putStrLn $ "-- This is a module parsed from file: " ++ G.getName mod_sum
                let hs_mod = G.unLoc $ G.pm_parsed_source parsed_mod
                G.prettyPrint hs_mod
                hsc_env <- G.getSession
                let hpm = G.HsParsedModule {
                            G.hpm_module = F.parsedSource parsed_mod
                            , G.hpm_src_files = F.pm_extra_src_files parsed_mod
                            , G.hpm_annotations = F.pm_annotations parsed_mod
                        }
                {-
                Fail: m_tge is Mothing.
                ((warns, errs), m_tge) <- liftIO $ TcRnDriver.tcRnModule hsc_env DriverPhases.HsSrcFile False hpm
                putStrLn "----- Warnings -----"
                liftIO $ ErrUtils.printBagOfErrors flags warns
                putStrLn "----- Errors -----"
                liftIO $ ErrUtils.printBagOfErrors flags errs
                -}
                {-
                -- Fails with exception.
                let warn_msgs = ErrUtils.emptyMessages
                liftIO $ HscTypes.runHsc hsc_env (HscMain.hscFileFrontEnd mod_sum)
                -}
                {-
                -- Fails with exception.
                G.hscTypecheckRename hsc_env mod_sum hpm
                tc_mod <- G.typecheckModule parsed_mod
                -}
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
