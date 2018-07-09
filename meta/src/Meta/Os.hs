{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Meta.Os (
    -- * Process
    module Meta.OsProc
    -- * Command-line arguments
    , Env.getProgName
    , Env.getArgs
    , Env.withProgName
    , Env.withArgs
    -- * Environment
    , Env.getEnv
    -- , Env.lookupEnv -- not in base 4.5
    , Env.getEnvironment
    -- * Careless transput
    , Slurp(..)
    -- * Directory traversal
    , descendant_files_of
) where

import Control.Monad ((>=>))

import qualified Control.Monad.IO.Class as MI
import qualified System.Directory.Tree as DT
import qualified System.Environment as Env

import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import qualified Data.Text.Encoding.Error as Tee

import Meta.OsProc

import qualified Meta.ByteString as B

{- |
The most general way to strictly and carelessly read a file.
-}
class Slurp m a where slurp :: FilePath -> m a

-- | 'B.readFile' from "Data.ByteString".
instance (MI.MonadIO m) => Slurp m B.ByteString where slurp = MI.liftIO . B.readFile
instance (MI.MonadIO m) => Slurp m B.LazyByteString where slurp = slurp >=> return . B.fromStrict
instance (MI.MonadIO m) => Slurp m T.Text where slurp = slurp >=> return . Te.decodeUtf8With Tee.lenientDecode
instance (MI.MonadIO m) => Slurp m String where slurp = slurp >=> return . T.unpack

{- |
Get every relative file path in the directory recursively.

Directories are excluded.

Unreadable files are silently ignored.
-}
descendant_files_of :: FilePath -> IO [FilePath]
descendant_files_of path = do
    anchor DT.:/ tree <- DT.readDirectoryWith read_contents path
    return $ map (\ name -> anchor ++ "/" ++ name) $ listify tree
    where
        read_contents _ = return ()
        listify :: DT.DirTree a -> [FilePath]
        listify node = case node of
            DT.File name _ -> [name]
            DT.Dir name children -> map (\ c -> name ++ "/" ++ c) (concatMap listify children)
