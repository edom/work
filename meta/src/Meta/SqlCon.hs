module Meta.SqlCon where

import qualified Control.Monad as M

import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as HP

import qualified Meta.Data_internal as DI
import qualified Meta.SqlType as T

{- |
See:

* 'HP.connectPostgreSQL'

* http://www.postgresql.org/docs/8.1/static/libpq.html#LIBPQ-CONNECT

* https://www.postgresql.org/docs/8.1/static/libpq-envars.html
-}
type ConStr = String

readNul :: String -> Either String Bool
readNul str = case str of
    "NO" -> pure False
    "YES" -> pure True
    _ -> Left $ "Meta.SqlCon.readNul: not implemented: " ++ str

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
                ignored = scm `elem` ignoredSchemas
            M.unless ignored $ do
                let rCol = do
                        ty <- T.from_sql typ
                        nu <- readNul nul
                        pure $ (DI.mkCol ty col) { DI._nullable = nu }
                putStrLn $ qna ++ ": " ++ col ++ " type " ++ typ ++ " nullable " ++ nul
                putStrLn $ " -> " ++ show rCol
        pure ()
    where
        -- HDBC doesn't support arrays
        query = "SELECT t.table_catalog, t.table_schema, t.table_name, c.column_name, c.is_nullable, c.data_type, c.character_maximum_length, c.numeric_precision, c.numeric_scale"
            ++ " FROM information_schema.tables t JOIN information_schema.columns c"
            ++ " ON (t.table_catalog, t.table_schema, t.table_name) = (c.table_catalog, c.table_schema, c.table_name)"
        string :: H.SqlValue -> String
        string = H.fromSql
        int :: H.SqlValue -> Int
        int = H.fromSql
        ignoredSchemas = [
                "information_schema"
                , "pg_catalog"
            ]