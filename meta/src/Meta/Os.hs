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
) where

import Control.Monad ((>=>))

import qualified Control.Monad.IO.Class as MI
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
instance (MI.MonadIO m) => Slurp m T.Text where slurp = slurp >=> return . Te.decodeUtf8With Tee.lenientDecode
instance (MI.MonadIO m) => Slurp m String where slurp = slurp >=> return . T.unpack
