{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
* Use cases

    * How do I parse a hs file?

        * Would I want to do it using GHC?

            * Why?

                * Stand on the shoulders of giants.
                Leverage all of GHC.

                * More likely to be supported than a random Hackage library.

                * Practically everyone uses GHC.
                Practically it is /the/ Haskell compiler.

                * Well-documented, for an open-source project.

            * Why not?

                * GHC is big.

                * Not everyone uses GHC.

                * Unstable.
                However, a big project such as GHC has high inertia, so the change shouldn't be very big.

                * The original vision of the project is programming language research, not real world usage.

                    * But people are already using it in production.

                * All support personnels are volunteers.

        * What are the alternatives?

            * <https://github.com/haskell-suite/haskell-src-exts>

                * How was it made?
                Did it incorporate any code from GHC?

    * How do I make an ADT (algebraic data type), and serialize it to hs source file?

        * How does GHC represent an ADT?

            * In "HsDecls", see the DataDecl constructor of TyClDecl, and see the HsDataDefn type.

* See also:

    * "Meta.Ghc"

    * Strathclyde Haskell Enhancement <https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/>
-}
module Meta.Hs where

import Prelude ()
import Meta.Prelude

import qualified Meta.Data as D
import qualified Meta.File as F
import qualified Meta.HsCon as C
import qualified Meta.HsMod as M
import qualified Meta.HsRender as HS
import qualified Meta.HsType as T
import qualified Meta.Type as MT

-- * Module

mkModule :: T.ModName -> [M.Member] -> M.Module
mkModule = M.mkModule

type Mod_name = T.ModName

-- * Render

renderModuleFile :: M.Module -> F.File
renderModuleFile = HS.renderModuleFile

-- * Generate DTO

genDto :: D.Table -> [M.Member]
genDto tab = [
        M.mDat datName [] cons [T.sc_Read, T.sc_Show]
    ]
    where
        tabName = D.t_get_name tab
        datName = "Table_" ++ tabName
        conName = "Mk" ++ datName
        cons = [C.CRec conName flds]
        flds = map mapColToFld cols
        cols = D.t_get_cols tab
        mapColToFld :: D.Col -> (T.VarName, T.Type)
        mapColToFld col = (tabName ++ "_" ++ D.c_get_name col, MT.data_to_hs $ D.c_get_type col)

-- * Experimental: Design for auto-lifting

val_unlifted :: Int
val_unlifted = 123

class (Applicative f) => Val f where val :: f Int
instance (Applicative f) => Val f where val = pure val_unlifted

fun_unlifted :: Int -> Int -> Int
fun_unlifted x y = (+) x y

infixl 0 `ap`
ap :: (Monad f) => f (a -> b) -> f a -> f b
ap mf ma = do
    f <- mf
    a <- ma
    return $ f a

-- ** A possibility: Metaprogram lifts all applications to ap

class (Monad f) => Fun1 f where fun1 :: f (Int -> Int -> Int)
instance (Monad f) => Fun1 f where fun1 = return fun_unlifted

res1 :: (Applicative f, Monad f) => f Int
res1 = fun1 `ap` val `ap` val

-- ** A possibility: Metaprogram generalizes liftM (compile-time liftM)
--
class (Monad f) => Fun2 f where fun2 :: f Int -> f Int -> f Int
instance (Monad f) => Fun2 f where fun2 mx my = mx >>= \ x -> my >>= \ y -> return (fun_unlifted x y)

res2 :: (Applicative f, Monad f) => f Int
res2 = fun2 val val
