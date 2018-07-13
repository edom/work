{- |
* Prerequisites

    * This requires GHC 8.

* What does this module do?

    * This gathers commonly used symbols from the @ghc@ package and @ghc-paths@ package.

    * This reexports them.

    * This also provides a convenient way to construct and manipulate GHC's syntax trees.

* Usage notes

    * If you need an instance 'G.GhcMonad' or 'G.ExceptionMonad', use 'G.Ghc'.
    It has 'IO' inside.

* Resources

    * <http://hackage.haskell.org/package/ghc>

    * <http://hackage.haskell.org/package/ghc-paths>

* See also

    * "Meta.Hs"
-}
module Meta.Ghc (
    -- * Simple interface
    -- ** Entry point
    defaultRunGhc
    , loadPackageDatabase
    , loadAllTargets
    -- ** Dynamic flags
    -- $dynflags
    , G.getSessionDynFlags
    , G.setSessionDynFlags
    -- ** Input
    -- $input
    , G.guessTarget
    , G.setTargets
    -- ** Pretty printing
    -- $pretty
    , Ou.showSDoc
    , Ou.ppr
    -- * Syntax tree
    -- $ast
    -- ** Reexport HsSyn
    , module S
    -- ** Locations and passes
    , G.noLoc
    , pass_GhcPs
    -- ** HsModule
    , emptyHsModule
    -- ** ADT: TyClDecl(DataDecl), HsDataDefn, ConDecl
    , emptyDataDecl
    , emptyHsDataDefn
    , emptyCon
    -- ** Names
    , SetName(..)
    -- *** Internal instances for making names
    , MkModuleName(..)
    , MkName(..)
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
) where

import HsSyn as S

import qualified DynFlags as DF
import qualified GHC as G
import qualified GHC.Paths as GP
import qualified Module as M
import qualified Outputable as Ou
import qualified OccName as Oc
import qualified RdrName as R

{- |
* This is 'G.runGhc' with recommended defaults.

* You may need to call 'loadPackageDatabase'.

* See also some sample codes:

    * <https://wiki.haskell.org/GHC/As_a_library>

    * <https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/extending_ghc.html>
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

{- $input
A /target/ is an input for the compiler.
It is read by the compiler.
-}

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

{- $ast
Convenience functions for constructing and manipulating GHC syntax trees.

In general, we follow these:

    * Everywhere a location is required, use 'G.noLoc'.

    * Everywhere a @('Maybe' a)@ is required, use 'Nothing'.
-}

-- | This helps the type-checker infer the compilation pass as \"parsed\".
pass_GhcPs :: f S.GhcPs -> f S.GhcPs
pass_GhcPs = id

-- | A default value for 'S.HsModule'.
-- Use 'pass_GhcPs' to help the type-checker infer the @pass@ argument.
emptyHsModule :: S.HsModule pass
emptyHsModule = S.HsModule {
        S.hsmodName = Nothing
        , S.hsmodExports = Nothing
        , S.hsmodImports = []
        , S.hsmodDecls = []
        , S.hsmodDeprecMessage = Nothing
        , S.hsmodHaddockModHeader = Nothing
    }

-- | Empty data type declaration.
emptyDataDecl :: G.TyClDecl G.GhcPs
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
emptyCon :: G.ConDecl G.GhcPs
emptyCon = G.ConDeclH98 {
        G.con_name = mkDataName ""
        , G.con_qvars = Nothing
        , G.con_cxt = Nothing
        , G.con_details = G.PrefixCon []
        , G.con_doc = Nothing
    }

-- | Set the name of @a@ to @b@.
class SetName a b where
    -- | @setName b a@ sets the name of @a@ to @b@.
    -- For example, @a@ may be a @'HsModule' 'G.GhcPs'@.
    setName :: b -> a -> a

instance SetName (G.HsModule G.GhcPs) String where
    setName name modu = modu {
            G.hsmodName = mkModuleName name
        }

instance SetName (G.TyClDecl G.GhcPs) String where
    setName name ddef = ddef {
            G.tcdLName = mkDataName name
        }

instance SetName (G.ConDecl G.GhcPs) String where
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
