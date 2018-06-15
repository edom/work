{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
See also Strathclyde Haskell Enhancement:

https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/
-}
module Meta.Hs where

import qualified Meta.HsCon as C
import qualified Meta.HsMod as M
import qualified Meta.HsType as T
import qualified Meta.Relat as R

-- * Generate DTO

genDto :: R.Table -> [M.Member]
genDto tab = [
        M.mDat datName cons [T.sc_Read, T.sc_Show]
    ]
    where
        tabName = R.tName tab
        datName = "Table_" ++ tabName
        conName = "Mk" ++ datName
        cons = [C.CRec conName flds]
        flds = map mapColToFld cols
        cols = R.tCols tab
        mapType :: R.Type -> T.Type
        mapType typ = case typ of
            R.TInt32 -> T.int32
            R.TInt64 -> T.int64
            R.TVarChar _ -> T.string
            _ -> error $ "Meta.Hs.genDto.mapType: " ++ show typ
        mapColToFld :: R.Col -> (T.VarName, T.Type)
        mapColToFld col = (tabName ++ "_" ++ R.cName col, mapType $ R.cType col)

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
    pure $ f a

-- ** A possibility: Metaprogram lifts all applications to ap

class (Monad f) => Fun1 f where fun1 :: f (Int -> Int -> Int)
instance (Monad f) => Fun1 f where fun1 = pure fun_unlifted

res1 :: (Monad m) => m Int
res1 = fun1 `ap` val `ap` val

-- ** A possibility: Metaprogram generalizes liftM (compile-time liftM)
--
class (Monad f) => Fun2 f where fun2 :: f Int -> f Int -> f Int
instance (Monad f) => Fun2 f where fun2 mx my = mx >>= \ x -> my >>= \ y -> pure (fun_unlifted x y)

res2 :: (Monad m) => m Int
res2 = fun2 val val
