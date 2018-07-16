{-# LANGUAGE CPP #-}

{- |
* Prerequisites

    * This requires GHC 8.2 or 8.4.

* What does this module do?

    * This gathers commonly used symbols from the @ghc@ package and @ghc-paths@ package.

    * This reexports them.

    * This also provides a convenient way to construct and manipulate GHC's syntax trees.

    * This presents a uniform interface for the supported GHC versions.

* Usage notes

    * If you need an instance 'G.GhcMonad' or 'G.ExceptionMonad', use 'G.Ghc'.
    It has 'IO' inside.

* Resources

    * <https://ghc.haskell.org/trac/ghc/wiki/GhcApiStatus>

    * <https://ghc.haskell.org/trac/ghc/wiki/NativeMetaprogramming>

    * <https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow>

    * 2008, email, \"[Haskell-cafe] GHC API: how to get the typechecked AST?\" <https://mail.haskell.org/pipermail/haskell-cafe/2008-May/042616.html>

    * 2005, article, \"Porting HaRe to the GHC API\" <https://www.cs.kent.ac.uk/pubs/2005/2266/>

    * Sample codes

        * <https://wiki.haskell.org/GHC/As_a_library>

        * <https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/extending_ghc.html>

* See also

    * Related modules in this package

        * "Meta.Hs"

    * Related packages on Hackage

        * <http://hackage.haskell.org/package/ghc>

        * <http://hackage.haskell.org/package/ghc-paths>

        * ghc-mod is a backend program to enrich Haskell programming in editors <http://hackage.haskell.org/package/ghc-mod>

        * hint: Runtime Haskell interpreter (GHC API wrapper) <http://hackage.haskell.org/package/hint>

        * intero: Complete interactive development program for Haskell <http://hackage.haskell.org/package/intero>

        * HaRe: the Haskell Refactorer. <http://hackage.haskell.org/package/HaRe>

    * Other related projects

        * <https://github.com/haskell/haskell-ide-engine>
-}
module Meta.Ghc (
    -- * Simple interface
    -- ** Entry point
    defaultRunGhc
    , loadPackageDatabase
    , G.GhcMonad(..)
    -- ** Dynamic flags
    -- $dynflags
    , G.getSessionDynFlags
    , G.setSessionDynFlags
    , setGeneralFlag
    -- ** Input
    -- $input
    , addFile
    , G.guessTarget
    , G.setTargets
    , loadAllTargets
    , G.SuccessFlag(..)
    , G.succeeded
    , G.failed
    , Z.getModuleGraph
    , Z.mgModSummaries
    , G.ModuleGraph
    , G.ModSummary(..)
    -- *** High-level functions for running phases
    , G.parseModule
    , G.typecheckModule
    , G.ParsedModule(..)
    , G.TypecheckedModule(..)
    , T.HsParsedModule(..)
    -- ** Pretty printing
    -- $pretty
    , prettyPrint
    , Ou.showSDoc
    , Ou.ppr
    -- * Syntax tree
    -- $ast
    -- ** Reexport HsSyn
    , module S
    -- ** Locations and passes
    , G.noLoc
    , G.unLoc
    , pass_GhcPs
    -- ** HsModule
    , emptyHsModule
    -- ** ADT: TyClDecl(DataDecl), HsDataDefn, ConDecl
    , emptyDataDecl
    , emptyHsDataDefn
    , emptyCon
    -- ** Names
    , GetName(..)
    , SetName(..)
    -- *** Internal instances for making names
    , MkModuleName(..)
    , MkName(..)
    -- * Experimental
    -- $experimental
    , newHscEnv
    , hscTypecheckRename
    -- * Other reexports
    -- ** Entry point
    , G.defaultErrorHandler
    , DF.defaultFatalMessager
    , DF.defaultFlushOut
    , DF.defaultLogAction
    , G.runGhc
    , GP.libdir
    -- ** Input
    , G.load
    , G.LoadHowMuch(..)
    -- * DynFlags, GeneralFlag, package databases
    , G.DynFlags(..)
    , G.GeneralFlag(..)
    , DF.PackageDBFlag(..)
    , DF.PkgConfRef(..)
    , DF.PackageFlag(..)
    , DF.PackageArg(..)
    , DF.ModRenaming(..)
    -- ** GhcLink, HscTarget
    , G.GhcLink(..)
    , G.HscTarget(..)
) where

import Prelude ()
import Meta.Prelude

import HsSyn as S

import qualified DynFlags as DF
import qualified GHC as G
import qualified GHC.Paths as GP
import qualified HscMain
import qualified Module as M
import qualified OccName as Oc
import qualified Outputable as Ou
import qualified RdrName as R
import qualified HscTypes as T

-- See the cabal file.
import qualified META_GHC_MODULE as Z

{- |
* This is 'G.runGhc' with recommended defaults.

* You may need to call 'loadPackageDatabase'.
-}
defaultRunGhc :: G.Ghc a -> IO a
defaultRunGhc com =
    G.defaultErrorHandler DF.defaultFatalMessager DF.defaultFlushOut $ do
        G.runGhc (Just GP.libdir) com

-- | You don't need to call this if you have already called 'G.setSessionDynFlags',
-- which has the side effect of loading the package database.
loadPackageDatabase :: G.Ghc [M.InstalledUnitId]
loadPackageDatabase = do
    flags <- G.getSessionDynFlags
    -- This has a side effect of loading the package database.
    G.setSessionDynFlags flags

{- |
See also 'G.load' and 'G.LoadAllTargets'.
-}
loadAllTargets :: (G.GhcMonad m) => m G.SuccessFlag
loadAllTargets = G.load G.LoadAllTargets

{- $dynflags
A /dynamic flag/ is a flag that can be modified from source file by OPTIONS_GHC pragma.
-}

setGeneralFlag :: G.GeneralFlag -> G.DynFlags -> G.DynFlags
setGeneralFlag = flip DF.gopt_set

{- $input
A /target/ is an input for the compiler.
It is read by the compiler.
-}

addFile :: (G.GhcMonad m) => FilePath -> m ()
addFile path = G.guessTarget path Nothing >>= G.addTarget

{- $pretty
* Everywhere you meet an instance of 'Ou.Outputable', you can transform it to 'Ou.SDoc' with 'Ou.ppr'.

    * Almost everything in "HsSyn" (syntax tree objects) is 'Ou.Outputable'.
    For example: 'S.HsModule' is.

* You can then transform an 'Ou.SDoc' into a pretty 'String' with 'Ou.showSDoc'.

    * It requires a 'DF.DynFlags' that can be obtained from 'G.getSessionDynFlags'.

    * For other ways of pretty-printing an 'Ou.SDoc', see 'Ou.SDoc'.

* This is similar to the @pretty@ package.

    * <http://hackage.haskell.org/package/pretty>

    * 1995, article, \"The design of a pretty-printing library\", John Hughes.
-}

prettyPrint :: (G.GhcMonad m, Ou.Outputable a) => a -> m ()
prettyPrint thing = do
    flags <- G.getSessionDynFlags
    putStrLn $ Ou.showSDoc flags $ Ou.ppr thing

{- $ast
Convenience functions for constructing and manipulating GHC syntax trees.

In general, we follow these:

    * Everywhere a location is required, use 'G.noLoc'.

    * Everywhere a @('Maybe' a)@ is required, use 'Nothing'.
-}

{- |
This is 'id', but this helps the type-checker infer the compilation pass as \"parsed\".

Some history:

* RdrName in GHC 8.2.

* GhcPs in GHC 8.4.
-}
pass_GhcPs :: f Z.GhcPs -> f Z.GhcPs
pass_GhcPs = id

-- | A default value for 'S.HsModule'.
-- Use 'pass_GhcPs' to help the type-checker infer the @pass@ argument.
emptyHsModule :: S.HsModule Z.GhcPs
emptyHsModule = S.HsModule {
        S.hsmodName = Nothing
        , S.hsmodExports = Nothing
        , S.hsmodImports = []
        , S.hsmodDecls = []
        , S.hsmodDeprecMessage = Nothing
        , S.hsmodHaddockModHeader = Nothing
    }

-- | Empty data type declaration.
emptyDataDecl :: G.TyClDecl Z.GhcPs
emptyDataDecl = G.DataDecl {
        G.tcdLName = mkDataName ""
        , G.tcdTyVars = G.mkHsQTvs []
        , G.tcdFixity = G.Prefix
        , G.tcdDataDefn = emptyHsDataDefn
        , G.tcdDataCusk = G.PlaceHolder
        , G.tcdFVs = G.PlaceHolder
    }

-- | Empty data type definition.
emptyHsDataDefn :: G.HsDataDefn pass
emptyHsDataDefn = G.HsDataDefn  {
        G.dd_ND = G.DataType
        , G.dd_ctxt = G.noLoc []
        , G.dd_cType = Nothing
        , G.dd_kindSig = Nothing
        , G.dd_cons = []
        , G.dd_derivs = G.noLoc []
    }

-- | Empty Haskell 98 data constructor.
emptyCon :: G.ConDecl Z.GhcPs
emptyCon = G.ConDeclH98 {
        G.con_name = mkDataName ""
        , G.con_qvars = Nothing
        , G.con_cxt = Nothing
        , G.con_details = G.PrefixCon []
        , G.con_doc = Nothing
    }

class GetName a b where
    getName :: a -> b

instance GetName G.ModSummary String where
    getName ms = G.moduleNameString $ G.moduleName $ G.ms_mod ms

-- | Set the name of @a@ to @b@.
class SetName a b where
    -- | @setName b a@ sets the name of @a@ to @b@.
    -- For example, @a@ may be a @'HsModule' 'G.GhcPs'@.
    setName :: b -> a -> a

instance SetName (G.HsModule Z.GhcPs) String where
    setName name modu = modu {
            G.hsmodName = mkModuleName name
        }

instance SetName (G.TyClDecl Z.GhcPs) String where
    setName name ddef = ddef {
            G.tcdLName = mkDataName name
        }

instance SetName (G.ConDecl Z.GhcPs) String where
    setName name cdec = case cdec of
        G.ConDeclGADT _ a b -> G.ConDeclGADT [mkDataName name] a b
        G.ConDeclH98 _ a b c d -> G.ConDeclH98 (mkDataName name) a b c d

-- | Convenience overloaded function 'mkModuleName'.
class MkModuleName a where
    -- | Convenience overloaded function for making something suitable for 'S.hsmodName' of 'S.HsModule'.
    -- Thus we can write @mod { hsmodName = mkModuleName \"TheName\" }@.
    -- But it is more convenient to use 'SetName' instead of such record update.
    mkModuleName :: String -> a

instance MkModuleName G.ModuleName where
    mkModuleName = G.mkModuleName

instance (MkModuleName a) => MkModuleName (G.Located a) where
    mkModuleName = G.noLoc . mkModuleName

instance (MkModuleName a) => MkModuleName (Maybe a) where
    mkModuleName = Just . mkModuleName

-- | Make a name.
class MkName a where

    -- | Make a type\/constructor name. The string should begin with an uppercase character.
    mkDataName :: String -> a

    -- | Make a variable name. The string should begin with a lowercase character.
    mkVarName :: String -> a

instance MkName R.RdrName where
    mkDataName = R.mkRdrUnqual . Oc.mkDataOcc
    mkVarName = R.mkRdrUnqual . Oc.mkVarOcc

instance (MkName a) => MkName (G.Located a) where
    mkDataName = G.noLoc . mkDataName
    mkVarName = G.noLoc . mkVarName

{- $experimental
* <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeChecker>

* <https://ghc.haskell.org/trac/ghc/wiki/NewPlugins>

    * <https://github.com/thoughtpolice/strict-ghc-plugin>

* How do we typecheck a module, but don't quit on error?

    * Salvage some things from 'G.typecheckModule'.
-}

{- |
See 'G.HscEnv'.

Why do we obtain a HscEnv?
We want to run a certain part of the typechecker.
we want to do something on some type errors.
'G.typecheckModule' is too coarse-grained.

Don't do this.
Use 'G.getSession' to obtain the HscEnv.
See the code of 'G.typecheckModule'.
-}
newHscEnv :: (G.GhcMonad m) => m G.HscEnv
newHscEnv = G.getSessionDynFlags >>= liftIO . HscMain.newHscEnv

-- hscTypecheckRename :: (G.GhcMonad m) => G.HscEnv -> G.ModSummary -> T.HsParsedModule -> m (TcGblEnv, _)
hscTypecheckRename env mod_sum hpm = liftIO $ HscMain.hscTypecheckRename env mod_sum hpm
