{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Meta.SqlCon (
    test
) where

import qualified Control.Exception as E
import qualified Data.String as DS

import qualified Database.PostgreSQL.Simple as P
import qualified Database.PostgreSQL.Simple.FromField as PF
import qualified Database.PostgreSQL.Simple.FromRow as PR

import qualified Meta.SqlInf as SI

instance (PF.FromField a) => SI.FromField PR.RowParser a where
    field = PR.field

with :: P.ConnectInfo -> (P.Connection -> IO a) -> IO a
with inf = E.bracket (P.connect inf) P.close

test :: IO ()
test = do
    with inf $ \ con -> do
        rows <- P.queryWith_ SI.parse con (DS.fromString SI.query)
        mapM_ (putStrLn . describe) $ SI.process rows
    where
        describe :: SI.Table -> String
        describe SI.MkTable{..} = _tSchema ++ "." ++ _tName
        -- Expect libpq to read these from environment variables PGHOST, PGDATABASE, and PGUSER.
        -- Expect the password to be in the pgpass file.
        inf = P.defaultConnectInfo {
                P.connectHost = ""
                , P.connectDatabase = ""
                , P.connectUser = ""
            }
