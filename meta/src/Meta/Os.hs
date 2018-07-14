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
    -- $careless
    , Slurp(..)
    -- ** Directory traversal
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

{- $careless
The only consideration for these careless functions are ease of use.
Errors are not handled.
Resource\/performance (CPU and memory usage) is not considered.
-}

{- |
The most general way to strictly and carelessly read a file.

Stability: implicit inference only.
Do not define your own instances.
Do not use in type signatures.
-}
class Slurp m a where
    {- |
Sloppily read the whole file into memory.

The parameter @m@ can be any instance of 'MI.MonadIO'.

The parameter @a@ can be 'B.ByteString', 'B.LazyByteString', 'T.Text', or 'String'.
    -}
    slurp :: FilePath -> m a

-- | 'B.readFile' from "Data.ByteString".
instance (MI.MonadIO m) => Slurp m B.ByteString where slurp = MI.liftIO . B.readFile
instance (MI.MonadIO m) => Slurp m B.LazyByteString where slurp = slurp >=> return . B.fromStrict
instance (MI.MonadIO m) => Slurp m T.Text where slurp = slurp >=> return . Te.decodeUtf8With Tee.lenientDecode
instance (MI.MonadIO m) => Slurp m String where slurp = slurp >=> return . T.unpack

{- |
Get every relative file path in the directory recursively.

Directories are excluded.

If any files are unreadable, the behahior is undefined.
Currently those unreadable files are silently ignored, but don't depend on this behavior.
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
            _ -> [] -- ignore read failures
