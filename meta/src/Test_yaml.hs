module Test_yaml where

import qualified Control.Monad as M
import qualified System.IO.Error as Ie

import qualified Data.Text as T

import qualified Data.Yaml as Y

data Db
    = Mk_db
    {
        d_name :: String
        , d_url :: String
        , d_username :: String
        , d_password :: String
    }
    deriving (Read, Show)

data Table
    = Mk_table
    {
        t_name :: String
        , t_columns :: [Column]
    }
    deriving (Read, Show)

data Column
    = Mk_column
    {
        c_name :: String
        , c_type :: Type
    }
    deriving (Read, Show)

data Type
    = Int
    | Text
    deriving (Read, Show)

{- |
http://www.liquibase.org/2007/06/the-problem-with-database-diffs.html

explicit renaming information

diff :: State -> State -> Diff
@diff a b@ is the patch that has to be applied to change @a@ to @b@.

apply :: Diff -> IO a
-}
data Syntax
    = Create_user String String
    | Alter_user String String
    | Get_user String String
    deriving (Read, Show)

class Grammar m where
    create_user :: String -> String -> m ()

foo :: IO ()
foo = do
    x <- Y.decodeFileEither path
        >>= either (Ie.ioError . Ie.userError . Y.prettyPrintParseException) return
    let p a = do
            dbs <- a Y..: T.pack "databases"
            M.forM dbs $ \ d -> Mk_db
                <$> d Y..: T.pack "name"
                <*> d Y..: T.pack "url"
                <*> d Y..: T.pack "username"
                <*> d Y..: T.pack "password"
    o <- either (Ie.ioError . Ie.userError) return $ Y.parseEither p x
    print (o :: [Db])
    return ()
    where
        path = "postgres.yml"
