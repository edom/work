{-# LANGUAGE CPP #-}

module Meta.SqlCon_hp where

#ifdef HAVE_postgresql

import qualified Control.Monad as M

import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as HP

import qualified Meta.Data_internal as DI
import qualified Meta.SqlInf as SI
import qualified Meta.SqlType as T

{- |
See:

* 'HP.connectPostgreSQL'

* <http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT>

* <https://www.postgresql.org/docs/8.1/static/libpq-envars.html>
-}
type ConStr = String

test :: IO ()
test = do
    HP.withPostgreSQL "" $ \ con -> do
        rows <- H.quickQuery con query []
        M.forM_ rows $ \ [cat_, scm_, tab_, col_, nul_, typ_, cml_, nup_, nus_] -> do
            let cat = string cat_
                scm = string scm_
                tab = string tab_
                col = string col_
                nul = string nul_
                typ = string typ_
                cml = int cml_
                nup = int nup_
                nus = int nus_
                qna = cat ++ "." ++ scm ++ "." ++ tab
                ignored = scm `elem` SI.ignoredSchemas
            M.unless ignored $ do
                let rCol = do
                        ty <- T.from_sql typ
                        nu <- SI.readNul nul
                        pure $ (DI.mkCol ty col) { DI._nullable = nu }
                putStrLn $ qna ++ ": " ++ col ++ " type " ++ typ ++ " nullable " ++ nul
                putStrLn $ " -> " ++ show rCol
        pure ()
    where
        query = SI.query
        string :: H.SqlValue -> String
        string = H.fromSql
        int :: H.SqlValue -> Int
        int = H.fromSql

#endif
