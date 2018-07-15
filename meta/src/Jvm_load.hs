{- |
This module deals with loading classes without initializing them.
-}
module Jvm_load
where

import Prelude ()
import Meta.Prelude

import qualified Control.Monad as Mo
import qualified System.IO.Error as Ie

import qualified Control.Monad.IO.Class as Ic

import qualified Data.ByteString.UTF8 as Bu

import qualified Meta.JvmArch as A
import qualified Meta.JvmCls as Z
import qualified Meta.JvmSer as S
import qualified Jvm_prepare as Prep

-- * Loading classes

class Load m where

    {- |
This does not run the class initializer.

This does not add the class to the list of loaded classes.
    -}
    load_class :: A.Class_name -> m A.Class

instance Load A.S where

    load_class = A.get_loaded_class

instance Load A.J where

    load_class name = do
        classpath <- A.gets A.s_classpath
        m <- A.find_loaded_class name
        case m of
            Just x -> return x
            _ -> load_class_in name classpath

-- * Loading classes in classpath

load_class_in :: (A.Stateful m, Ic.MonadIO m) => A.Class_name -> [FilePath] -> m A.Class
load_class_in name [] = A.stop (A.Class_not_found name)
load_class_in name (dir : rest) = do
    -- FIXME SECURITY path traversal
    -- TODO use filepath package
    let path = dir ++ "/" ++ Bu.toString name ++ ".class"
    debug_load path
    ec <- Ic.liftIO $ Ie.tryIOError $ load_class_file path
    case either Left Right ec of
        Left e | Ie.isDoesNotExistError e -> load_class_in name rest
        Left e -> A.stop (A.Failed_loading_class name (show e))
        Right u -> case either Left Right u of
            Left e -> A.stop (A.Failed_loading_class name e)
            Right c -> return c
    where
        debug_load path = do
            debug <- A.gets A.s_debug_load
            Mo.when debug $ Ic.liftIO $ putStrLn $ "[debug] [Jvm_load] [load_class] Trying " ++ path

-- * Internals

{- |
This returns a 'A.Class' from "Meta.JvmArch", not a 'Z.Class' from "Meta.JvmCls".

You are not supposed to use this function directly;
let the virtual machine load the classes on its own.
-}
load_class_file :: FilePath -> IO (EitherString A.Class)
load_class_file = fmap (>>= Prep.resolve_class) . parse_class_file

{- |
This returns a 'Z.Class' from "Meta.JvmCls", not a 'A.Class' from "Meta.JvmArch".

Deserialize the binary representation from disk.
-}
parse_class_file :: FilePath -> IO (EitherString Z.Class)
parse_class_file path = S.parse_class path <$> slurp path
