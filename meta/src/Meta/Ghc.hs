{- |
* Prerequisites

    * This requires GHC 8.

* What does this module do?

    * This gathers commonly used symbols from the @ghc@ package and @ghc-paths@ package.

    * This reexports them.

* Usage notes

    * If you need an instance 'G.GhcMonad' or 'G.ExceptionMonad', use 'G.Ghc'.
    It has 'IO' inside.

* Use cases?

    * How do I parse a hs file?

        * Would I want to do it using GHC?

* Resources

    * http://hackage.haskell.org/package/ghc

    * http://hackage.haskell.org/package/ghc-paths

    * Sample codes
-}
module Meta.Ghc (
    -- * Simple interface
    -- ** Entry point
    defaultRunGhc
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

import qualified DynFlags as DF
import qualified GHC as G
import qualified GHC.Paths as GP
import qualified Outputable as Ou

{- |
This is 'G.runGhc' with recommended defaults.

* See also some sample codes:

    * https://wiki.haskell.org/GHC/As_a_library

    * https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/extending_ghc.html
-}
defaultRunGhc :: G.Ghc a -> IO a
defaultRunGhc com = G.defaultErrorHandler DF.defaultFatalMessager DF.defaultFlushOut $ G.runGhc (Just GP.libdir) com

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
* This is similar to the @pretty@ package.

    * http://hackage.haskell.org/package/pretty

    * 1995, article, \"The design of a pretty-printing library\", John Hughes.
-}
